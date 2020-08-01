use std::iter::FusedIterator;

enum PossiblyExtendExtIteratorItem {
    First,
    Second,
    Done,
}

struct PossiblyExtendExtIterator {
    name: String,
    pos: PossiblyExtendExtIteratorItem,
}
impl<'a> PossiblyExtendExtIterator {
    fn new(name: String) -> Self {
        Self {
            name: name,
            pos: PossiblyExtendExtIteratorItem::First,
        }
    }
}
impl Iterator for PossiblyExtendExtIterator {
    type Item = String;
    fn next(&mut self) -> Option<String> {
        match self.pos {
            PossiblyExtendExtIteratorItem::First => {
                self.pos = PossiblyExtendExtIteratorItem::Second;
                Some(self.name.clone())
            }
            PossiblyExtendExtIteratorItem::Second => {
                self.pos = PossiblyExtendExtIteratorItem::Done;
                if self.name.ends_with(".source") {
                    None
                } else {
                    Some(self.name.clone() + ".source")
                }
            }
            PossiblyExtendExtIteratorItem::Done => None,
        }
    }
}

impl<'a> FusedIterator for PossiblyExtendExtIterator {}

fn possibly_append_ext(name: String) -> PossiblyExtendExtIterator {
    PossiblyExtendExtIterator::new(name)
}

pub type ResolveIter = Box<dyn Iterator<Item = String>>;

pub fn resolve(name: &str, current_filename: Option<&str>) -> ResolveIter {
    if name.contains("//") {
        // it is an absolute URL (maybe protocol-relative), so we don't prepend anything
        Box::new(possibly_append_ext(name.to_owned()))
    } else if name.starts_with('/') {
        // it is a domain-relative URL
        // firstly, try prepending the current domain name
        let domain: Option<&str> = current_filename.map(|curr| {
            let domain_start = curr.find("//").map_or(0, |l| l + "//".len());
            let domain = curr[domain_start..]
                .find('/')
                .map_or(curr, |domain_end_offset| {
                    &curr[..(domain_start + domain_end_offset)]
                });
            domain
        });
        // secondly, don't prepend anything
        if let Some(dom) = domain {
            Box::new(
                possibly_append_ext(String::from(dom) + name)
                    .chain(possibly_append_ext(name.to_owned())),
            )
        } else {
            Box::new(possibly_append_ext(name.to_owned()))
        }
    } else {
        // it is a relative URL
        // firstly, we resolve this URL in the scope of the current name
        let current_path_prefix: Option<&str> =
            current_filename.and_then(|curr| curr.rfind("/").map(|idx| &curr[..(idx + "/".len())]));
        let first_candidate: Option<String> =
            current_path_prefix.map(|pref| String::from(pref) + name);
        // secondly, resolve this as a stdlib name in the hardcoded standard library path
        let stdlib_name: String = String::from("https://btzy.github.io/libsourceror/") + name;
        // thirdly, don't prepend anything
        if let Some(fc) = first_candidate {
            Box::new(
                possibly_append_ext(fc)
                    .chain(possibly_append_ext(stdlib_name))
                    .chain(possibly_append_ext(name.to_owned())),
            )
        } else {
            Box::new(possibly_append_ext(stdlib_name).chain(possibly_append_ext(name.to_owned())))
        }
    }
}
