const fs = require('fs');

// fix the wasm package.json so Node accepts it
const packageJson = JSON.parse(fs.readFileSync("wasm/package.json", "utf8"));
packageJson.type = "module";
fs.writeFileSync("wasm/package.json", JSON.stringify(packageJson, undefined, 2));

// copy load-wasm.js to dist
fs.copyFileSync("src/load-wasm.js", "dist/load-wasm.js");
