// Mixin usages in PUG:

// +tex(false, false).
//     $\mathbf{Definition}$ (Space of Sections). Let $\mathbf{H}$ be
//     a $(\infty,1)$-topos, and let $E \rightarrow B : \mathbf{H}_{/B}$ a bundle in
//     $\mathbf{H}$, object in the slice topos. Then the space of sections $\Gamma_\Sigma(E)$
//     of this bundle is the Dependent Product.

// +tex(true, false).
//     $$
//         \Gamma_\Sigma(E) = \Pi_\Sigma (E) \in \mathbf{H}.
//     $$

// +code.
//     def Pi (A : U) (B : A → U) : U := Π (x : A), B x

const {mathjax} = require('mathjax-full/js/mathjax.js');
const {TeX} = require('mathjax-full/js/input/tex.js');
const {SVG} = require('mathjax-full/js/output/svg.js');
const {liteAdaptor} = require('mathjax-full/js/adaptors/liteAdaptor.js');
const {RegisterHTMLHandler} = require('mathjax-full/js/handlers/html.js');
const {AssistiveMmlHandler} = require('mathjax-full/js/a11y/assistive-mml.js');
const {AllPackages} = require('mathjax-full/js/input/tex/AllPackages.js');

const adaptor = liteAdaptor();
const handler = RegisterHTMLHandler(adaptor);

const tex = new TeX({
    packages: ['base', 'autoload', 'require', 'ams', 'amscd', 'newcommand', 'configmacros'],
    inlineMath: [ ["$", "$"] ],
    macros: {                                                                                       // Plug your Glyphs here
        llparenthesis: '\\mathopen{\u2987}',
        rrparenthesis: '\\mathclose{\u2988}',
        llbracket:     '\\mathopen{\u27E6}',
        rrbracket:     '\\mathclose{\u27E7}',
        incmap:        '\\mathclose{\u21AA}',
        meet:          '\\mathopen{\u2227}',
        map:           '\\mathopen{\u21A6}',
        join:          '\\mathopen{\u2228}',
        trans:         '\\, \\mathbin{\\vcenter{\\rule{.3ex}{.3ex}}} \\,',
        mapright:      ['\\xrightarrow{{#1}}', 1],
        mapdown:       ['\\Big\\downarrow\\rlap{\\raise2pt{\\scriptstyle{#1}}}', 1],
        mapdiagl:      ['\\vcenter{\\searrow}\\rlap{\\raise2pt{\\scriptstyle{#1}}}', 1],
        mapdiagr:      ['\\vcenter{\\swarrow}\\rlap{\\raise2pt{\\scriptstyle{#1}}}', 1],
    }
});

tex.postFilters.add(({math, data}) => {
    if (!data.error) return;
    data.root.walkTree((node) => {
        if (node.isKind('merror')) {
            console.log('TeX error:\n  ' + node.attributes.get('data-mjx-error'));
        }
    });
});

const svg = new SVG({fontCache: 'local'});

function renderPug(block) {
    var recv; with({pug_html: ""}){
        eval(`(${block})();`); recv = pug_html;
    }; return recv
}

function renderTeX(formulae) {
    return adaptor.innerHTML(mathjax.document(formulae, {
        InputJax: tex, OutputJax: svg
    }).render().document.body);
}

exports.tex = function (block) {
    return renderTeX(renderPug(block));
}

exports.highlight = function (block) {
    return renderPug(block)
        .replace(/([(){}→=]+|:|:=)/g,
            '<span class="h__symbol">$1</span>')
        .replace(/\b(∀|Π|Σ|W|λ|glue|unglue|Glue|transp|hcomp|where|def|begin|end|module|import|option|false|true|indᵂ|sup|.1|.2|𝟎|𝟏|𝟐|ind₂|ind₁|ind₀|★|0₂|1₂|PathP|PartialP|inc|ouc|axiom|theorem|lemdata|ma|U|V)\b(?!:)/g,
            '<span class="h__keyword">$1</span>');
}
