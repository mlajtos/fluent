import { serve } from "bun";
import index from "./client.html";

const server = serve({
  routes: {
    "/": index,
  },
  development: true
});

console.log(`Listening on ${server.url}`);