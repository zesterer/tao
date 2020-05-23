const code = document.querySelector("#code");

import("../pkg/index.js")
  .then((module) => {
    document.querySelector("#run").addEventListener("click", () => {
      document.querySelector("#output").innerHTML = "";

      try {
        document.querySelector("#output").innerHTML += module.run(
          code.innerText
        );
      } catch (errors) {
        let output = "";

        output += '<span style="color: red">';

        output += errors.length;

        if (errors.length == 1) {
          output += " error when compiling \n\n";
        } else {
          output += " errors when compiling \n\n";
        }

        errors.forEach((err) => {
          output += err;
          output += "\n\n";
        });
        output += "<span>";
        document.querySelector("#output").innerHTML = output;
      }
    });
  })
  .catch(console.error);

document.querySelector("#save").addEventListener("click", () => {
  save("playground.tao");
});

document.querySelector("#load").addEventListener("change", (event) => {
  const file = event.target.files[0];

  const reader = new FileReader();
  reader.addEventListener("load", (event) => {
    code.innerText = event.target.result;
  });
  reader.readAsText(file);
});

// Function to download data to a file
function save(filename) {
  var file = new Blob([code.innerText], {
    type: "text/plain",
  });
  if (window.navigator.msSaveOrOpenBlob)
    // IE10+
    window.navigator.msSaveOrOpenBlob(file, filename);
  else {
    // Others
    var a = document.createElement("a"),
      url = URL.createObjectURL(file);
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    setTimeout(function () {
      document.body.removeChild(a);
      window.URL.revokeObjectURL(url);
    }, 0);
  }
}

const themes = [
  { background_color: "darkslategray", code_color: "wheat" },
  { background_color: "#292d3e", code_color: "#99c2eb" },
];
const themes_toolbar = document.querySelector("#themes");

for (var i = 0; i < themes.length; i++) {
  let svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("class", "theme-btn");
  svg.setAttribute("theme", i);
  svg.setAttribute("height", 30);
  svg.setAttribute("width", 30);

  svg.innerHTML = `<polygon
  points="0,0 30,0 30,30 0,30"
  style="fill: ${themes[i].background_color};"
/>`;

  svg.onclick = () => {
    document.documentElement.style.setProperty(
      "--code-background",
      themes[svg.getAttribute("theme")].background_color
    );
    document.documentElement.style.setProperty(
      "--code-color",
      themes[svg.getAttribute("theme")].code_color
    );
  };

  themes_toolbar.appendChild(svg);
}
