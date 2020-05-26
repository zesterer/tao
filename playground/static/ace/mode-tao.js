ace.define(
  "ace/mode/tao_highlight_rules",
  [
    "require",
    "exports",
    "module",
    "ace/lib/oop",
    "ace/mode/text_highlight_rules",
  ],
  function (require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    var TextHighlightRules = require("./text_highlight_rules")
      .TextHighlightRules;

    var TaoHighlightRules = function () {
      var keywords =
        "def|match|if|else|then|null|let|and|or|given|in|as|with|of|type|data";

      var builtinConstants = "true|false";

      var keywordMapper = this.createKeywordMapper(
        {
          keyword: keywords,
          "constant.language": builtinConstants,
        },
        "identifier"
      );

      this.$rules = {
        start: [
          {
            token: "comment",
            regex: "#.*$",
          },
          {
            token: "string", // " string
            regex: '".*?"',
          },
          {
            token: "char", // ' char
            regex: "'.*?'",
          },
          {
            token: "constant.numeric", // float
            regex: "\\d+(?:(?:\\.\\d*)?)?\\b",
          },
          {
            token: "keyword.operator",
            regex:
              "\\+|\\-|\\/|\\*|\\%|<|>|<=|>=|=>|!=|\\+\\+|\\.\\.\\.|!",
          },
          {
            token: keywordMapper,
            regex: "[a-zA-Z_][a-zA-Z0-9_]*",
          },
          {
            token: "paren.lparen",
            regex: "[\\(]",
          },
          {
            token: "paren.rparen",
            regex: "[\\)]",
          },
          {
            token: "paren.lbracket",
            regex: "[\\[]",
          },
          {
            token: "paren.rbracket",
            regex: "[\\]]",
          },
          {
            token: "pipe",
            regex: "[\\|]",
          },
          {
            token: "colon",
            regex: "[:]",
          },
          {
            token: "text",
            regex: "\\s+",
          },
        ],
      };
      this.normalizeRules();
    };

    oop.inherits(TaoHighlightRules, TextHighlightRules);

    exports.TaoHighlightRules = TaoHighlightRules;
  }
);

ace.define(
  "ace/mode/tao",
  [
    "require",
    "exports",
    "module",
    "ace/lib/oop",
    "ace/mode/text",
    "ace/mode/tao_highlight_rules",
  ],
  function (require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    var TextMode = require("./text").Mode;
    var TaoHighlightRules = require("./tao_highlight_rules").TaoHighlightRules;

    var Mode = function () {
      this.HighlightRules = TaoHighlightRules;
      this.$behaviour = this.$defaultBehaviour;
    };
    oop.inherits(Mode, TextMode);

    (function () {
      this.lineCommentStart = "#";

      this.$id = "ace/mode/tao";
    }.call(Mode.prototype));

    exports.Mode = Mode;
  }
);
(function () {
  ace.require(["ace/mode/tao"], function (m) {
    if (typeof module == "object" && typeof exports == "object" && module) {
      module.exports = m;
    }
  });
})();
