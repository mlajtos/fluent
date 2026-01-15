import { serve, file } from "bun";
import index from "./index.html";

const server = serve({
  routes: {
    "/": index,
    "/monaco.worker.js": file("./monaco.worker.js"),
  },
  development: true
});

console.log(`Listening on ${server.url}`);
