import "./index.css";

var editor = ace.edit("code");
editor.setOptions({
  fontFamily: "Fira Code",
  fontSize: "12pt",
});
editor.setTheme("ace/theme/monokai");
editor.session.setNewLineMode("unix");
editor.setShowPrintMargin(false);
editor.session.setMode("ace/mode/tao");

var Range = ace.require("ace/range").Range;
var marker_ids = [];

if (window.Worker) {
  let worker = new Worker("./worker.js");

  worker.onmessage = function (result) {
    document.querySelector("#output").innerHTML = "";

    if (result.data.errors == null) {
      document.querySelector("#output").innerText += result.data.out;
    } else {
      let errors = result.data.errors;

      handle_errors(errors);
    }
  };

  worker.onerror = () => {
    alert("Crash");
  };

  document.querySelector("#run").addEventListener("click", () => {
    clearMarkers();
    worker.postMessage(editor.getValue());
  });
} else {
  import("../pkg/index.js")
    .then((module) => {
      document.querySelector("#run").addEventListener("click", () => {
        clearMarkers();
        document.querySelector("#output").innerHTML = "";

        try {
          document.querySelector("#output").innerText += module.run(
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
  document.querySelector("#output").innerHTML = "";

  let span = document.createElement("span");
  span.style.color = "red";
  span.style.whiteSpace = "pre-wrap";

  span.innerHTML += errors.length;

  if (errors.length == 1) {
    span.innerHTML += " error when compiling \n\n";
  } else {
    span.innerHTML += " errors when compiling \n\n";
  }

  for (let i = 0; i < errors.length; i++) {
    const err = errors[i];

    for (let k = 0; k < err.src.error.primary_spans.length; k++) {
      const span = err.src.error.primary_spans[k].Range;

      let start = editor.session.doc.indexToPosition(span[0]);
      let end = editor.session.doc.indexToPosition(span[1]);

      marker_ids.push(
        editor.session.addMarker(
          new Range(start.row, start.column, end.row, end.column),
          "error",
          "line",
          true
        )
      );
    }

    span.innerText += err.msg;

    span.innerHTML += "\n\n";
  }

  document.querySelector("#output").append(span);
}

function clearMarkers() {
  let id = marker_ids.pop();

  while (id != undefined) {
    editor.session.removeMarker(id);
    id = marker_ids.pop();
  }
}

editor.commands.removeCommand("find");

// String splice polyfill
String.prototype.splice = function (idx, rem, str) {
  return this.slice(0, idx) + str + this.slice(idx + Math.abs(rem));
};

(async function () {
  const examples = document.querySelector(".examples");
  try {
    const response = await fetch(
      "https://api.github.com/repos/zesterer/tao/contents/examples"
    );

    if (!response.ok) {
      throw null;
    }

    examples.innerHTML = "";

    let contents = await response.json();

    for (let i = 0; i < contents.length; i++) {
      const element = contents[i];

      if (element.type === "file") {
        let li = document.createElement("li");
        li.classList.add("link");
        li.innerText = element.name;
        li.onclick = () => {
          loadExample(element.url);
        };
        examples.append(li);
      }
    }
  } catch (err) {
    examples.innerHTML = "";

    let li = document.createElement("li");
    li.innerText = "(╯°□°)╯︵ ┻━┻ error getting examples";
    examples.append(li);
  }
})();

async function loadExample(url) {
  const response = await fetch(url);

  if (!response.ok) {
    return;
  }

  let file = await response.json();
  let data = atob(file.content);

  editor.setValue(data);

  document.querySelector(".overlay").style.display = "none";
}

document.querySelector("#open").onclick = () => {
  document.querySelector(".overlay").style.display = "block";
};

document.querySelector(".overlay").onclick = () => {
  document.querySelector(".overlay").style.display = "none";
};

document.querySelector(".explorer").onclick = (e) => {
  e.stopPropagation();
};
