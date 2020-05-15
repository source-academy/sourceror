use crate::error::DepError;
use crate::error::FetcherError;
use crate::error::GraphError;
use async_trait::async_trait;
use projstd::log::CompileMessage;
use projstd::log::SourceLocationRef as plSLRef;
use std::boxed::Box;
use std::collections::HashMap;
use std::future::Future;
use std::iter::DoubleEndedIterator;
use std::iter::ExactSizeIterator;
use std::iter::FusedIterator;
use std::pin::Pin;
use std::result::Result;

#[async_trait]
pub trait Fetcher<T> {
    async fn fetch(&self, name: &str, sl: plSLRef<'_>) -> Result<T, CompileMessage<FetcherError>>;
}

pub trait ExtractDeps<'a> {
    type Iter: Iterator<Item = (&'a str, plSLRef<'a>)>;
    fn extract_deps(&self, filename: Option<&'a str>) -> Self::Iter;
}

struct GraphNode<T> {
    deps: Vec<usize>, // indices into Graph::nodes
    content: T,
    name: Option<String>,
}

pub struct Graph<T> {
    nodes: Vec<GraphNode<T>>,
}

impl<T> Graph<T>
where
    for<'a> T: ExtractDeps<'a>,
{
    // Will ensure that nodes with larger index will only depend on nodes with smaller index
    // So the largest index will be the given `t` (root)
    pub async fn try_async_build_from_root<F: Fetcher<T>>(
        t: T,
        f: &F,
    ) -> Result<Self, CompileMessage<DepError>> {
        let mut graph = Graph::<T> { nodes: Vec::new() };
        // Note: for some reasons async functions are not allowed to be recursive,
        // so we do a DFS using a separate vector as the 'stack'.
        let mut cache = HashMap::<String, Option<usize>>::new();
        let mut deps = Vec::new();
        for (dep, sl) in t.extract_deps(None) {
            deps.push(
                graph
                    .get_or_fetch_node_recursive(dep, sl, &mut cache, f)
                    .await?,
            );
        }
        let idx = graph.nodes.len();
        graph.nodes.push(GraphNode {
            deps: deps,
            content: t,
            name: None,
        });
        Ok(graph)
    }
    fn get_or_fetch_node_recursive<'b, F: Fetcher<T>>(
        &'b mut self,
        name: &'b str,
        sl: plSLRef<'b>,
        cache: &'b mut HashMap<String, Option<usize>>,
        f: &'b F,
    ) -> Pin<Box<dyn 'b + Future<Output = Result<usize, CompileMessage<DepError>>>>> {
        Box::pin(async move {
            if let Some(opt_idx) = cache.get(name) {
                if let Some(idx) = opt_idx {
                    return Ok(*idx);
                }
                return Err(CompileMessage::new_error(sl.to_owned(), GraphError {}).into_cm());
            }
            match f.fetch(name, sl).await {
                Err(e) => Err(e.into_cm()),
                Ok(t) => {
                    cache.insert(name.to_owned(), None);
                    let mut deps = Vec::new();
                    for (dep, sl) in t.extract_deps(Some(name)) {
                        deps.push(self.get_or_fetch_node_recursive(dep, sl, cache, f).await?);
                    }
                    let idx = self.nodes.len();
                    self.nodes.push(GraphNode {
                        deps: deps,
                        content: t,
                        name: Some(name.to_owned()),
                    });
                    *cache.get_mut(name).unwrap() = Some(idx);
                    Ok(idx)
                }
            }
        })
    }
}

impl<T> Graph<T> {
    // Returns an iterator that traverses the dependency tree in a valid topological ordering.
    // When traversing file A, all dependencies of A must have already been traversed, and the state returned by each of its dependencies will be provided (immutably since there might be diamonds)
    pub fn topological_traverse(
        &self,
    ) -> impl DoubleEndedIterator<Item = (&T, Option<&str>)>
           + ExactSizeIterator<Item = (&T, Option<&str>)>
           + FusedIterator<Item = (&T, Option<&str>)> {
        self.nodes
            .iter()
            .map(|node| (&node.content, node.name.as_deref()))
    }
    pub fn topological_traverse_state_into<
        S,
        F: FnMut(usize, Box<[&S]>, T, Option<String>) -> S,
    >(
        self,
        f: F,
    ) {
        let mut states: Vec<S> = Vec::new();
        states.reserve(self.nodes.len());
        for (i, node) in self.nodes.into_iter().enumerate() {
            let depstates: Box<[&S]> = node.deps.into_iter().map(|x| &states[x]).collect();
            states[i] = f(i, depstates, node.content, node.name);
        }
    }

    /*
    pub fn topological_traverse<S: Clone + Combine>(
        &self,
        state: &S,
    ) -> impl DoubleEndedIterator<Item = (&[usize], &T, &str)>
           + ExactSizeIterator<Item = (&[usize], &T, &str)>
           + FusedIterator<Item = (&[usize], &T, &str)> {
        self.nodes
            .iter()
            .map(|node| (node.deps.as_slice(), &node.content, node.name.as_str()))
    }
    pub fn topological_traverse_into<S: Clone + Combine>(
        self,
        state: &S,
    ) -> impl DoubleEndedIterator<Item = (Vec<usize>, T, String)>
           + ExactSizeIterator<Item = (Vec<usize>, T, String)>
           + FusedIterator<Item = (Vec<usize>, T, String)> {
        self.nodes
            .into_iter()
            .map(|node| (node.deps, node.content, node.name))
    }
    */
}
