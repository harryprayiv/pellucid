import { readFileSync } from "node:fs";

// spago test runs from the project root.
export const fixture = readFileSync("test/fixtures/home.json", "utf8");
export const relationshipFixture = readFileSync("test/fixtures/relationship.json", "utf8");
export const contextFixture = readFileSync("test/fixtures/context.json", "utf8");
export const notificationsFixture = readFileSync("test/fixtures/notifications.json", "utf8");
export const composeSettingsFixture = readFileSync("test/fixtures/compose_settings.json", "utf8");
export const mediaUploadFixture = readFileSync("test/fixtures/media_upload.json", "utf8");
export const searchFixture = readFileSync("test/fixtures/search.json", "utf8");
export const hashtagFixture = readFileSync("test/fixtures/hashtag.json", "utf8");
export const followedTagsFixture = readFileSync("test/fixtures/followed_tags.json", "utf8");
export const trendingTagsFixture = readFileSync("test/fixtures/trending_tags.json", "utf8");
export const conversationsFixture = readFileSync("test/fixtures/conversations.json", "utf8");
export const threadFixture = readFileSync("test/fixtures/thread.json", "utf8");
export const messageSentFixture = readFileSync("test/fixtures/message_sent.json", "utf8");
