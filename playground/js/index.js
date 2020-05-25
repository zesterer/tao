var editor = ace.edit("code");
editor.setOptions({
  fontFamily: "Fira Code",
  fontSize: "12pt",
});
editor.setTheme("ace/theme/monokai");
editor.session.setNewLineMode("unix");
editor.setShowPrintMargin(false);
editor.session.setMode("ace/mode/tao");

// String splice polyfill
String.prototype.splice = function (idx, rem, str) {
  return this.slice(0, idx) + str + this.slice(idx + Math.abs(rem));
};

if (window.Worker) {
  let worker = new Worker("./worker.js");

  worker.onmessage = function (result) {
    document.querySelector("#output").innerHTML = "";

    if (result.data.errors == null) {
      document.querySelector("#output").innerHTML += result.data.out;
    } else {
      let errors = result.data.errors;

      handle_errors(errors);
    }
  };

  document.querySelector("#run").addEventListener("click", () => {
    worker.postMessage(editor.getValue());
  });
} else {
  import("../pkg/index.js")
    .then((module) => {
      document.querySelector("#run").addEventListener("click", () => {
        document.querySelector("#output").innerHTML = "";

        try {
          document.querySelector("#output").innerHTML += module.run(
            editor.getValue()
          );
        } catch (errors) {
          handle_errors(errors);
        }
      });
    })
    .catch(console.error);
}

document.querySelector("#save").addEventListener("click", () => {
  save("playground.tao");
});

document.querySelector("#load").addEventListener("change", (event) => {
  const file = event.target.files[0];

  const reader = new FileReader();
  reader.addEventListener("load", (event) => {
    editor.setValue(event.target.result);
  });
  reader.readAsText(file);
});

// Function to download data to a file
function save(filename) {
  var file = new Blob([editor.getValue()], {
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

function handle_errors(errors) {
  let output = "";

  console.log(errors);

  output += '<span style="color: red">';

  output += errors.length;

  if (errors.length == 1) {
    output += " error when compiling \n\n";
  } else {
    output += " errors when compiling \n\n";
  }

  //let highlighting_text = document.querySelector("#code").innerText;

  for (let i = 0; i < errors.length; i++) {
    const err = errors[i];

    /*     let opening_tag = '<span class="highlight-err">';
    
    let closing_tag = "</span>";

    for (let j = 0; j < err.src.error.primary_spans.length; j++) {
      const range = err.src.error.primary_spans[j].Range;
      
      console.log("-------");
      console.log(range);
      
      highlighting_text = highlighting_text.splice(range[0], 0, opening_tag);
      highlighting_text = highlighting_text.splice(
        range[1] + opening_tag.length,
        0,
        closing_tag
      );
      
      for (let k = j; k < err.src.error.primary_spans.length; k++) {
        const offset = opening_tag.length + closing_tag.length;
        console.log(err.src.error.primary_spans[k].Range);
        err.src.error.primary_spans[k].Range[0] += offset;
        err.src.error.primary_spans[k].Range[1] += offset;
        console.log(err.src.error.primary_spans[k].Range);
      }
    } */

    output += err.msg;

    output += "\n\n";
  }

  output += "<span>";
  document.querySelector("#output").innerHTML = output;
}

/* const themes = [
                { background_color: "darkslategray", code_color: "wheat" },
                { background_color: "#292d3e", code_color: "#99c2eb" },
              ];
              const themes_toolbar = document.querySelector("#themes");
              
              if (localStorage.getItem("theme") != null) {
                document.documentElement.style.setProperty(
                  "--code-background",
                  themes[localStorage.getItem("theme")].background_color
                );
                document.documentElement.style.setProperty(
                  "--code-color",
                  themes[localStorage.getItem("theme")].code_color
                );
              }
              
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
              
                  localStorage.setItem("theme", svg.getAttribute("theme"));
                };
              
                themes_toolbar.appendChild(svg);
              } */
