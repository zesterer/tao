const code = document.querySelector("#code");

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
    worker.postMessage(document.querySelector("#code").innerText);
  });
} else {
  import("../pkg/index.js")
    .then((module) => {
      document.querySelector("#run").addEventListener("click", () => {
        document.querySelector("#output").innerHTML = "";

        try {
          document.querySelector("#output").innerHTML += module.run(
            code.innerText
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
}

function handle_errors(errors) {
  let output = "";

  output += '<span style="color: red">';

  output += errors.length;

  if (errors.length == 1) {
    output += " error when compiling \n\n";
  } else {
    output += " errors when compiling \n\n";
  }

  errors.forEach((err) => {
    output += err.msg;

    console.log(err.src);

    output += "\n\n";
  });
  output += "<span>";
  document.querySelector("#output").innerHTML = output;
}

document.querySelector("#code").onkeydown = function (e) {
  if (e.keyCode === 9) {
    // tab key
    e.preventDefault(); // this will prevent us from tabbing out of the editor

    // now insert four non-breaking spaces for the tab key
    var editor = document.querySelector("#code");
    var doc = editor.ownerDocument.defaultView;
    var sel = doc.getSelection();
    var range = sel.getRangeAt(0);

    var tabNode = document.createTextNode("\u0009");
    range.insertNode(tabNode);

    range.setStartAfter(tabNode);
    range.setEndAfter(tabNode);
    sel.removeAllRanges();
    sel.addRange(range);
  } else if (e.keyCode === 13) {
    // enter key
    e.preventDefault();

    var editor = document.querySelector("#code");
    var doc = editor.ownerDocument.defaultView;
    var sel = doc.getSelection();
    var range = sel.getRangeAt(0);

    var enterNode = document.createElement("br");
    range.insertNode(enterNode);

    range.setStartAfter(enterNode);
    range.setEndAfter(enterNode);
    sel.removeAllRanges();
    sel.addRange(range);
  }
};
