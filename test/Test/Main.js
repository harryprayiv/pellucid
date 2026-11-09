import { readFileSync } from "node:fs";

// spago test runs from the project root.
export const fixture = readFileSync("test/fixtures/home.json", "utf8");
export const relationshipFixture = readFileSync("test/fixtures/relationship.json", "utf8");
export const contextFixture = readFileSync("test/fixtures/context.json", "utf8");
export const notificationsFixture = readFileSync("test/fixtures/notifications.json", "utf8");
export const composeSettingsFixture = readFileSync("test/fixtures/compose_settings.json", "utf8");
export const mediaUploadFixture = readFileSync("test/fixtures/media_upload.json", "utf8");
