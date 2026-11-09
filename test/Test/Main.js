import { readFileSync } from "node:fs";

// spago test runs from the project root.
export const fixture = readFileSync("test/fixtures/home.json", "utf8");
export const relationshipFixture = readFileSync("test/fixtures/relationship.json", "utf8");
export const contextFixture = readFileSync("test/fixtures/context.json", "utf8");