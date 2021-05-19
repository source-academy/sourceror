/**
 * This file contains a caching system for imported modules,
 * so subsequent re-compilation will be faster.
 */

const cache = new Map();

export function cachedGetFile(name, resolver) {
  const existing_val = cache.get(name);
  if (existing_val) return existing_val;
  const new_val = resolver(name);
  cache.set(name, new_val);
  return new_val;
}

