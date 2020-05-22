import("../pkg/index.js")
  .then((module) => {
    document.querySelector("#run").addEventListener("click", () => {
      document.querySelector("#output").innerHTML = "";

      try {
        document.querySelector("#output").innerHTML += module.run(document.querySelector("#code").value);
      } catch (errors) {
        let output = "";

        output += "<span style=\"color: red\">"
        errors.forEach(err => {
          output += err;
          output += "\n";
        });
        output += "<\span>";
        document.querySelector("#output").innerHTML = output;
      }
    });
  })
  .catch(console.error);
