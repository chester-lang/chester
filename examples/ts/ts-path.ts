import * as path from "node:path";
function joinPath(base: string,  name: string): string {
    return path.join(base,  name);
};
function main() {
    return joinPath("/tmp",  "demo.txt");
};
main();