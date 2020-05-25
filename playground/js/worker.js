import("../pkg/index.js")
  .then((module) => {
    self.addEventListener("message", (ev) => {
      try {
        let res = module.run(ev.data);

        self.postMessage({ out: res });
      } catch (errors) {
        self.postMessage({ errors: errors });
      }
    });
  })
  .catch(console.error);
