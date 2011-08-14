/*
 * TO DO: Fix syntax recognition to use proper Haskell tokenization.  Currently
 * this gets fairly close due to some special casing of common stuff, but it's
 * not really correct.
 */
define('ace/mode/haskell', function(require, exports, module) {
    var oop = require('pilot/oop');
    var TextMode = require('ace/mode/text').Mode;
    var Tokenizer = require('ace/tokenizer').Tokenizer;
    var HaskellHighlightRules = require('ace/mode/haskell_highlight_rules').HaskellHighlightRules;

    var Mode = function() {
        this.$tokenizer = new Tokenizer(new HaskellHighlightRules().getRules());
    };
    oop.inherits(Mode, TextMode);

    (function() {
    }).call(Mode.prototype);

    exports.Mode = Mode;
});

define('ace/mode/haskell_highlight_rules', function(require, exports, module) {
    var oop = require('pilot/oop');
    var lang = require('pilot/lang');
    var unicode = require('ace/unicode');

    var TextHighlightRules = require('ace/mode/text_highlight_rules').TextHighlightRules;

    var HaskellHighlightRules = function() {
        var keywords = lang.arrayToMap(
            ("case|of|class|data|family|default|deriving|do|forall|" +
             "foreign|if|then|else|import|infix|infixl|infixr|instance|" +
             "let|in|module|newtype|type|where|_").split("|"));

        var variableRe    = '[' + unicode.packages.Ll + "_][" + unicode.packages.L + unicode.packages.Nd + "_\\']*\\b";
        var constructorRe = '[' + unicode.packages.Lu + "][" + unicode.packages.L + "_\\']*\\b";
        var keywordOpRe   = '(?:\\.\\.)|(?:\\:)|(?:\\:\\:)|(?:\\=)|(?:\\\\)|'
                          + '(?:\\|)|(?:\\<\\-)|(?:\\-\\>)|(?:\\@)|(?:\\~)|'
                          + '(?:\\=\\>)|(?:\\;)|(?:\\,)'
        var symbol        = "[\\!\\#\\$\\%\\&\\*\\+\\.\\/\\<\\=\\>\\?\\@\\\\\\^\\|\\-\\~\\:]"
        var nsymbol       = "[^\\!\\#\\$\\%\\&\\*\\+\\.\\/\\<\\=\\>\\?\\@\\\\\\^\\|\\-\\~\\:]"
        var lsymbol       = "[\\!\\#\\$\\%\\&\\*\\+\\.\\/\\<\\=\\>\\?\\@\\\\\\^\\|\\-\\~]"
        this.$rules = {
            "start": [
                {
                    token : "comment",
                    regex : "\\-\\-\\-*(?:(?:$)|(?:"+nsymbol+".*$))"
                },
                {
                    token : "comment.doc",
                    regex : "\\{\\-\\#",
                    next  : "comment-doc"
                },
                {
                    token : "comment",
                    regex : "\\{\\-",
                    next  : "comment"
                },
                {
                    token : function(value) {
                        if (keywords.hasOwnProperty(value)) return "keyword";
                        else return "identifier";
                    },
                    regex : variableRe
                },
                {
                    token : "variable",
                    regex : constructorRe
                },
                {
                    token : "keyword.operator",
                    regex : keywordOpRe
                },
                {
                    token : "operator",
                    regex : lsymbol + symbol + "+"
                },
                {
                    token : "operator",
                    regex : ":" + symbol + "+"
                },
                {
                    token : "lparen",
                    regex : "[[({]"
                },
                {
                    token : "rparen",
                    regex : "[\\])}]"
                },
                {
                    token : "constant.numeric",
                    regex : "[" + unicode.packages.Nd + "]+\\.[" + unicode.packages.Nd + "]+(?:[eE][\\+\\-]?[" + unicode.packages.Nd + "]+)?"
                },
                {
                    token : "constant.numeric",
                    regex : "[" + unicode.packages.Nd + "]+[eE][\\+\\-]?[" + unicode.packages.Nd + "]+"
                },
                {
                    token : "constant.numeric",
                    regex : "0[Oo][" + unicode.packages.Nd + "]+"
                },
                {
                    token : "constant.numeric",
                    regex : "0[Xx][" + unicode.packages.Nd + "abcdefABCDEF]+"
                },
                {
                    token : "constant.numeric",
                    regex : "[" + unicode.packages.Nd + "]+"
                },
                {
                    token : "string",
                    regex : '\\"(?:(?:\\.)|[^\\"])*\\"'
                },
                {
                    token : "string",
                    regex : "\\'(?:(?:\\.)|[^\\'])*\\'"
                }
            ],
            "comment": [
                {
                    token : "comment",
                    regex : ".*?\\-\\}",
                    next  : "start"
                },
                {
                    token : "comment",
                    regex : ".+",
                    merge : true
                }
            ],
            "comment-doc": [
                {
                    token : "comment.doc",
                    regex : ".*?\\-\\}",
                    next  : "start"
                },
                {
                    token : "comment.doc",
                    regex : ".+",
                    merge : true
                }
            ]
        };
    }
    oop.inherits(HaskellHighlightRules, TextHighlightRules);

    exports.HaskellHighlightRules = HaskellHighlightRules;
});

