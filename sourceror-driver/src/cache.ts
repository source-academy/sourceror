/**
 * This file contains a caching system for imported modules,
 * so subsequent re-compilation will be faster.
 */

const cache: Map<string, Promise<string>> = new Map();

export function cachedGetFile(name: string, resolver: (name: string) => Promise<string>): Promise<string> {
  const existing_val = cache.get(name);
  if (existing_val) return existing_val;
  const new_val = resolver(name);
  cache.set(name, new_val);
  return new_val;
}
