import { readFileSync } from "node:fs";

// spago test runs from the project root.
export const fixture = readFileSync("test/fixtures/home.json", "utf8");
