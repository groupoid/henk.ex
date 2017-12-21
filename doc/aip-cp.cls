%%
%% This is file `aip-cp.cls',
%% generated with the docstrip utility.
%%
%% Copyright (c) 2014 American Institute of Physics (AIP).
%% 
%% \CharacterTable
%%  {Upper-case    \A\B\C\D\E\F\G\H\I\J\K\L\M\N\O\P\Q\R\S\T\U\V\W\X\Y\Z
%%   Lower-case    \a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z
%%   Digits        \0\1\2\3\4\5\6\7\8\9
%%   Exclamation   \!     Double quote  \"     Hash (number) \#
%%   Dollar        \$     Percent       \%     Ampersand     \&
%%   Acute accent  \'     Left paren    \(     Right paren   \)
%%   Asterisk      \*     Plus          \+     Comma         \,
%%   Minus         \-     Point         \.     Solidus       \/
%%   Colon         \:     Semicolon     \;     Less than     \<
%%   Equals        \=     Greater than  \>     Question mark \?
%%   Commercial at \@     Left bracket  \[     Backslash     \\
%%   Right bracket \]     Circumflex    \^     Underscore    \_
%%   Grave accent  \`     Left brace    \{     Vertical bar  \|
%%   Right brace   \}     Tilde         \~}
%%
\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{aip-cp}[2014/12/10 v1.0 
 Standard LaTeX document class for AIP Conference Proceedings]
\newcommand\@ptsize{}
\newif\if@restonecol
\newif\if@titlepage
%% \newif\if@numberedheadings
\@titlepagefalse
\if@compatibility\else
\DeclareOption{a4paper}
   {\setlength\paperheight {297mm}%
    \setlength\paperwidth  {210mm}}
\DeclareOption{a5paper}
   {\setlength\paperheight {210mm}%
    \setlength\paperwidth  {148mm}}
\DeclareOption{b5paper}
   {\setlength\paperheight {250mm}%
    \setlength\paperwidth  {176mm}}
\DeclareOption{letterpaper}
   {\setlength\paperheight {11in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{legalpaper}
   {\setlength\paperheight {14in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{executivepaper}
   {\setlength\paperheight {10.5in}%
    \setlength\paperwidth  {7.25in}}
\DeclareOption{landscape}
   {\setlength\@tempdima   {\paperheight}%
    \setlength\paperheight {\paperwidth}%
    \setlength\paperwidth  {\@tempdima}}
%%
\DeclareOption{numberedheadings}{\AtEndOfClass{\setcounter{secnumdepth}{4}}}%\@numberedheadingstrue}
%%
\fi
\if@compatibility
  \renewcommand\@ptsize{0}
\else
\DeclareOption{10pt}{\renewcommand\@ptsize{0}}
\fi
\DeclareOption{11pt}{\renewcommand\@ptsize{1}}
\DeclareOption{12pt}{\renewcommand\@ptsize{2}}
\if@compatibility\else
\DeclareOption{oneside}{\@twosidefalse \@mparswitchfalse}
\fi
\DeclareOption{twoside}{\@twosidetrue  \@mparswitchtrue}
\DeclareOption{draft}{\setlength\overfullrule{5pt}}
\if@compatibility\else
\DeclareOption{final}{\setlength\overfullrule{0pt}}
\fi
\DeclareOption{titlepage}{\@titlepagetrue}
\if@compatibility\else
\DeclareOption{notitlepage}{\@titlepagefalse}
\fi
\if@compatibility\else
\DeclareOption{onecolumn}{\@twocolumnfalse}
\fi
\DeclareOption{twocolumn}{\@twocolumntrue}
\DeclareOption{leqno}{\input{leqno.clo}}
\DeclareOption{fleqn}{\input{fleqn.clo}}
\DeclareOption{openbib}{%
  \AtEndOfPackage{%
   \renewcommand\@openbib@code{%
      \advance\leftmargin\bibindent
      \itemindent -\bibindent
      \listparindent \itemindent
      \parsep \z@
      }%
   \renewcommand\newblock{\par}}%
}
\ExecuteOptions{letterpaper,10pt,oneside,onecolumn,final}
\ProcessOptions
%\input{size1\@ptsize.clo}
%%
%%======== Inputs ===========
\usepackage{txfonts,graphicx}

\renewcommand{\sfdefault}{phv}
\renewcommand{\ttdefault}{cmtt}

\setlength\topmargin{0pt}
\setlength\headheight{0\p@}
\setlength\headsep   {0\p@}
\setlength\topskip   {0\p@}
\setlength\footskip{0\p@}

\textheight=52pc
\textwidth=39pc

\def\@xviiipt{18}
\def\@xvipt{14}

\renewcommand\normalsize{%
   \@setfontsize\normalsize\@xpt{11.5}
   \abovedisplayskip 10\p@ \@plus2\p@ \@minus5\p@
   \abovedisplayshortskip \z@ \@plus3\p@
   \belowdisplayshortskip 6\p@ \@plus3\p@ \@minus3\p@
   \belowdisplayskip \abovedisplayskip
   \let\@listi\@listI}
\normalsize

\newcommand\small{%
   \@setfontsize\small\@ixpt{11}%
   \abovedisplayskip 8.5\p@ \@plus3\p@ \@minus4\p@
   \abovedisplayshortskip \z@ \@plus2\p@
   \belowdisplayshortskip 4\p@ \@plus2\p@ \@minus2\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 4\p@ \@plus2\p@ \@minus2\p@
               \parsep 2\p@ \@plus\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}
\newcommand\footnotesize{%
   \@setfontsize\footnotesize\@viiipt{9.5}%
   \abovedisplayskip 6\p@ \@plus2\p@ \@minus4\p@
   \abovedisplayshortskip \z@ \@plus\p@
   \belowdisplayshortskip 3\p@ \@plus\p@ \@minus2\p@
   \def\@listi{\leftmargin\leftmargini
               \topsep 3\p@ \@plus\p@ \@minus\p@
               \parsep 2\p@ \@plus\p@ \@minus\p@
               \itemsep \parsep}%
   \belowdisplayskip \abovedisplayskip
}

\newcommand\scriptsize{\@setfontsize\scriptsize\@viipt\@viiipt}
\newcommand\tiny{\@setfontsize\tiny\@vpt\@vipt}
\newcommand\large{\@setfontsize\large\@xiipt{14}}
\newcommand\Large{\@setfontsize\Large\@xivpt{18}}
\newcommand\LARGE{\@setfontsize\LARGE\@xviiipt{20}}
\newcommand\huge{\@setfontsize\huge\@xxpt{25}}
\newcommand\Huge{\@setfontsize\Huge\@xxvpt{30}}
\newcommand\abstractfont{\@setfontsize\abstractfont\@ixpt{10.25}}
\newcommand\authorfont{\@setfontsize\authorfont\@xvipt{16}}
\newcommand\captionfont{\@setfontsize\captionfont\@ixpt{10.5}}

%% \setlength\footnotesep{6.65\p@}
%% \setlength{\skip\footins}{9\p@ \@plus 4\p@ \@minus 2\p@}
%% \setlength\floatsep    {12\p@ \@plus 2\p@ \@minus 2\p@}
%% \setlength\textfloatsep{20\p@ \@plus 2\p@ \@minus 4\p@}
%% \setlength\intextsep   {12\p@ \@plus 2\p@ \@minus 2\p@}
%% \setlength\dblfloatsep    {12\p@ \@plus 2\p@ \@minus 2\p@}
%% \setlength\dbltextfloatsep{20\p@ \@plus 2\p@ \@minus 4\p@}
%% \setlength\@fptop{0\p@ \@plus 1fil}
%% \setlength\@fpsep{8\p@ \@plus 2fil}
%% \setlength\@fpbot{0\p@ \@plus 1fil}
%% \setlength\@dblfptop{0\p@ \@plus 1fil}
%% \setlength\@dblfpsep{8\p@ \@plus 2fil}
%% \setlength\@dblfpbot{0\p@ \@plus 1fil}
\setlength\partopsep{2\p@ \@plus 1\p@ \@minus 1\p@}
\def\@listi{\leftmargin\leftmargini
            \parsep 0\p@ \@plus2\p@ \@minus\p@
            \topsep 8\p@ \@plus2\p@ \@minus4\p@
            \itemsep0\p@ \@plus2\p@ \@minus\p@}
\let\@listI\@listi
\@listi
\def\@listii {\leftmargin\leftmarginii
              \labelwidth\leftmarginii
              \advance\labelwidth-\labelsep
              \topsep    0\p@ \@plus2\p@ \@minus\p@
              \parsep    0\p@ \@plus\p@  \@minus\p@
              \itemsep   \parsep}
\def\@listiii{\leftmargin\leftmarginiii
              \labelwidth\leftmarginiii
              \advance\labelwidth-\labelsep
              \topsep    2\p@ \@plus\p@\@minus\p@
              \parsep    \z@
              \partopsep \p@ \@plus\z@ \@minus\p@
              \itemsep   \topsep}
\def\@listiv {\leftmargin\leftmarginiv
              \labelwidth\leftmarginiv
              \advance\labelwidth-\labelsep}
\def\@listv  {\leftmargin\leftmarginv
              \labelwidth\leftmarginv
              \advance\labelwidth-\labelsep}
\def\@listvi {\leftmargin\leftmarginvi
              \labelwidth\leftmarginvi
              \advance\labelwidth-\labelsep}

\def\enumerate{%
  \ifnum \@enumdepth >\thr@@\@toodeep\else
    \advance\@enumdepth\@ne
    \edef\@enumctr{enum\romannumeral\the\@enumdepth}%
    \labelsep1em%
      \expandafter
      \list
        \csname label\@enumctr\endcsname
        {\usecounter\@enumctr\def\makelabel##1{\hss\llap{##1}}}%
  \fi}

\def\itemize{%
  \ifnum \@itemdepth >\thr@@\@toodeep\else
    \advance\@itemdepth\@ne
    \edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
    \labelsep14pt%
    \expandafter
    \list
      \csname\@itemitem\endcsname
      {\def\makelabel##1{\hss\llap{##1}}}%
  \fi}

\def\SetLabel#1{\@bsphack\protected@write\@auxout{}%
         {\string\AIPlabel{#1}{\@currentlabel}}\@esphack}
\def\AIPlabel#1#2{\expandafter\xdef\csname X@#1\endcsname{#2}}
\def\SetRef#1{\@ifundefined{X@#1}{??}{\csname X@#1\endcsname}}

\def\numberofauthors{0}
\def\check@authorcount{\advance\eadrcount\m@ne\immediate\write\@auxout{\string\gdef\string\numberofauthors{\the\eadrcount}}}%
\def\aipauthors{}
\let\authorsep\@empty
\let\sep\@empty
\newcounter{authorcount}
\newcount\eadrcount
\def\author{\@ifnextchar[{\@@author}{\@author}}

\def\@@author[#1]#2{\stepcounter{authorcount}%
  \g@addto@macro\aipauthors{\normalsize\authorfont%
    \authorsep#2\unskip\textsuperscript{%
      \@for\@@addmark:=#1\do{%
       \edef\addnum{\@ifundefined{X@\@@addmark}{??}{\SetRef{\@@addmark}}}%
       \unskip\sep\addnum\let\sep=,}%
       \global\advance\eadrcount\@ne%
       \ifx\@corref\@empty\else\unskip\sep\@corref\let\sep=,\fi%
       \@ifundefined{r@ead@ref@\romannumeral\the\eadrcount}{}{\unskip\sep\ref{ead@ref@\romannumeral\the\eadrcount}}%
       \ifx\@noteref\@empty\else%
         \@for\@@notemark:=\@noteref\do{%
         \edef\notenum{\@ifundefined{X@\@@notemark}{??}{\SetRef{\@@notemark}}}%
         \unskip\sep\notenum\let\sep=,}%
       \fi%  
       }%
    \ifnum\numberofauthors=\the\eadrcount\def\authorsep{and\space}\else\def\authorsep{\unskip,\space}\fi%
     \global\let\sep\@empty\global\let\@corref\@empty\global\let\@noteref\@empty%
}}

\def\@author#1{\stepcounter{authorcount}%
  \g@addto@macro\aipauthors{\normalsize\authorfont%
    \upshape\authorsep#1\unskip\textsuperscript{%
       \global\advance\eadrcount\@ne%
       \ifx\@corref\@empty\else\unskip\sep\@corref\let\sep=,\fi%
       \ifnum\numberofauthors>\z@\@ifundefined{r@ead@ref@\romannumeral\the\eadrcount}{}{\unskip\sep\ref{ead@ref@\romannumeral\the\eadrcount}}\fi%
       \ifx\@noteref\@empty\else%
         \@for\@@notemark:=\@noteref\do{%
         \edef\notenum{\@ifundefined{X@\@@notemark}{??}{\SetRef{\@@notemark}}}%
         \unskip\sep\notenum\let\sep=,}%
       \fi%  
      }%
    \ifnum\numberofauthors=\the\eadrcount\def\authorsep{and\space}\else\def\authorsep{\unskip,\space}\fi%
    \global\let\@corref\@empty
    \global\let\@noteref\@empty%
    \global\let\sep\@empty}%
}

\def\aipaffil{}
\def\addsep{\par\vskip6pt}
\def\affil{\@ifnextchar[{\@@affil}{\@affil}}
\newcounter{affn}
\renewcommand\theaffn{\arabic{affn}}

\long\def\@@affil[#1]#2{\g@addto@macro\aipaffil{\normalsize\itshape%
     \refstepcounter{affn}
     \xdef\@currentlabel{\theaffn}
     \SetLabel{#1}%
    \textsuperscript{\rm\theaffn}#2\par}}

\long\def\@affil#1{\g@addto@macro\aipaffil{\normalsize\itshape%
    #1\par}}%

\let\@correspmark\@empty
\let\@correspnotes\@empty
\newcounter{cornote}
\renewcommand\thecornote{\alph{cornote})}
\def\corresp{\@ifnextchar[{\@@corresp}{\@corresp}}

\def\@@corresp[#1]#2{\g@addto@macro\aipaffil{\reset@font%
    \refstepcounter{cornote}%
    \ifnum\c@cornote=\@ne\par\vskip1em\fi%
    \xdef\@currentlabel{\thecornote}%
    \SetLabel{#1}%
    \textsuperscript{\thecornote}#2\par}}

\def\@corresp#1{\g@addto@macro\aipaffil{\reset@font%
    \refstepcounter{cornote}%
    \ifnum\c@cornote=\@ne\par\vskip1em\fi%
    #1\par}}

\let\@corref\@empty
\def\corref#1{\edef\@corref{\SetRef{#1}}}

\def\aunotes{}
%% Added on 09th Aug. 2017
\def\@fnsymbol#1{\ensuremath{\ifcase#1\or *\or \dagger\or **\or
   \ddagger\or \mathsection\or \mathparagraph\or \|\or \dagger\dagger
   \or \ddagger\ddagger \or\mathsection\mathsection
   \or \mathparagraph\mathparagraph \or *{*}*\or
   \dagger{\dagger}\dagger \or\ddagger{\ddagger}\ddagger\or
   \mathsection{\mathsection}\mathsection
   \or \mathparagraph{\mathparagraph}\mathparagraph \else\@ctrerr\fi}}
%%
\newcounter{authornote}
\renewcommand\theauthornote{\fnsymbol{authornote}}
\def\authornote{\@ifnextchar[{\@@authornote}{\@authornote}}

\def\@@authornote[#1]#2{\g@addto@macro\aunotes{\reset@font%
    \refstepcounter{authornote}%
    \xdef\@currentlabel{\theauthornote}%
    \SetLabel{#1}%
    \ifnum\c@authornote=\@ne\hspace*{-.8em}\else\par\fi\textsuperscript{\theauthornote}#2}}

\def\@authornote#1{\g@addto@macro\aunotes{\reset@font%
    \refstepcounter{authornote}%
    \ifnum\c@authornote=\@ne\hspace*{-.8em}\else\par\fi#1}}

\let\@noteref\@empty
\def\noteref#1{\gdef\@noteref{#1}}%

\def\eaddress{\@ifnextchar[{\@urladd}{\@emailadd}}
\def\@urladd[#1]#2{\expandafter\xdef\csname aip@eaddress\romannumeral\theauthorcount\endcsname{URL:\space#2}}%
\def\@emailadd#1{\expandafter\xdef\csname aip@eaddress\romannumeral\theauthorcount\endcsname{#1}}%
\newcount\tempcount%
\newcount\tempacount%
\def\print@eaddress{%
  \ifnum\c@authorcount>\z@%
  \ifnum\c@cornote=\z@\par\vskip1em\fi%
  \tempcount\z@%
  \tempacount=\c@authorcount%
  \loop
    \ifnum\tempacount>\z@
      \advance\tempcount\@ne%
        \@ifundefined{aip@eaddress\romannumeral\the\tempcount}{}{%
           \ifnum\numberofauthors>\z@%
              \stepcounter{cornote}%
              \xdef\@currentlabel{\thecornote}
              \label{ead@ref@\romannumeral\the\tempcount}%
              \textsuperscript{\thecornote}\csname aip@eaddress\romannumeral\the\tempcount\endcsname\par%
           \else%
              \csname aip@eaddress\romannumeral\the\tempcount\endcsname\par%
           \fi%  
          }%
        \global\advance\tempacount\m@ne%
  \repeat%
 \fi%
}
%%%%%%%%%%
\AtBeginDocument{%
  \newcounter{tabnotecount}[table]%
  \renewcommand\thetabnotecount{\fnsymbol{tabnotecount}}%
  \setlength\labelsep{19.5pt}% for reference number sep
}

%% \def\PrintTableNotes{}%
%% \def\tablenote{\@ifnextchar[{\@@tablenote}{\@tablenote}}
\newcommand\tabnotefont{\normalfont}
\def\@tablenote#1{\g@addto@macro\PrintTableNotes{\removespaces\tabnotefont\leftskip6pt#1\par}}
\def\@@tablenote[#1]#2{%
  \g@addto@macro\PrintTableNotes{\tabnotefont\leftskip6pt%
    \refstepcounter{tabnotecount}%
    \xdef\@currentlabel{\thetabnotecount}%
    \label{#1}%
    {\textsuperscript{\thetabnotecount\hskip4pt}}#2\par}%
}

\def\tabnotesep{,}%
\def\print@tabnote#1,#2\@nil{%
  \def\@tmp{#2}%
  \ifx\@tmp\@empty%
    \textsuperscript{{\ref{#1}}}%
  \else%
    \textsuperscript{{\ref{#1}}\tabnotesep}%
    \expandafter\print@tabnote#2\@nil%
  \fi%
}

\def\tabnoteref#1{\expandafter\print@tabnote#1,\@nil}

\newenvironment{tabnote}{\par\vskip2pt}{\par}%

\newcommand\tch[4]{\multicolumn{#1}{#2}%
   {\tableheadfont\begin{tabular}[#3]{@{}#2@{}}%
    \ignorespaces#4\unskip
    \end{tabular}}}

%% \def\toprule{\noalign{\kern2pt}\hline\noalign{\kern2pt}}
%% \def\colrule{\noalign{\kern2pt}\hline\noalign{\kern2pt}}%{\\[-10pt]\hline\\[-8pt]}
%% \def\botrule{\noalign{\kern2pt}\hline}%{\\[-10pt]\hline}

\def\hline{%
  \noalign{\ifnum0=`}\fi\vskip2pt
      \hrule \@height \arrayrulewidth\vskip2pt
      \futurelet \reserved@a\@xhline}
%%
%%======== End Inputs ===========
%%
\setlength\lineskip{1\p@}
\setlength\normallineskip{1\p@}
\renewcommand\baselinestretch{}
\setlength\parskip{0\p@ \@plus \p@}
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301
\setcounter{topnumber}{2}
\renewcommand\topfraction{.7}
\setcounter{bottomnumber}{1}
\renewcommand\bottomfraction{.3}
\setcounter{totalnumber}{3}
\renewcommand\textfraction{.2}
\renewcommand\floatpagefraction{.5}
\setcounter{dbltopnumber}{2}
\renewcommand\dbltopfraction{.7}
%\renewcommand\dblfloatpagefraction{.5}
\renewcommand\dblfloatpagefraction{.901}%
\if@twoside
  \def\ps@headings{%
      \let\@oddfoot\@empty\let\@evenfoot\@empty
      \def\@evenhead{\thepage\hfil\slshape\leftmark}%
      \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
      \let\@mkboth\markboth
    \def\sectionmark##1{%
      \markboth {\MakeUppercase{%
        \ifnum \c@secnumdepth >\z@
          \thesection\quad
        \fi
        ##1}}{}}%
    \def\subsectionmark##1{%
      \markright {%
        \ifnum \c@secnumdepth >\@ne
          \thesubsection\quad
        \fi
        ##1}}}
\else
  \def\ps@headings{%
    \let\@oddfoot\@empty
    \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \let\@mkboth\markboth
    \def\sectionmark##1{%
      \markright {\MakeUppercase{%
        \ifnum \c@secnumdepth >\m@ne
          \thesection\quad
        \fi
        ##1}}}}
\fi
\def\ps@myheadings{%
    \let\@oddfoot\@empty\let\@evenfoot\@empty
    \def\@evenhead{\thepage\hfil\slshape\leftmark}%
    \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \let\@mkboth\@gobbletwo
    \let\sectionmark\@gobble
    \let\subsectionmark\@gobble
    }
  \if@titlepage
  \newcommand\maketitle{\begin{titlepage}%
  \let\footnotesize\small
  \let\footnoterule\relax
  \let \footnote \thanks
  \null\vfil
  \vskip 60\p@
  \begin{center}%
    {\LARGE \@title \par}%
    \vskip 3em%
    {\large
     \lineskip .75em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}\par}%
      \vskip 1.5em%
    {\large \@date \par}%       % Set date in \large size.
  \end{center}\par
  \@thanks
  \vfil\null
  \end{titlepage}%
  \setcounter{footnote}{0}%
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
}
\else
\newcommand\maketitle{\par
  \begingroup
    \renewcommand\thefootnote{\@fnsymbol\c@footnote}%
    \def\@makefnmark{\rlap{\@textsuperscript{\normalfont\@thefnmark}}}%
    \long\def\@makefntext##1{\parindent 1em\noindent
            \hb@xt@1.8em{%
                \hss\@textsuperscript{\normalfont\@thefnmark}}##1}%
    \if@twocolumn
      \ifnum \col@number=\@ne
        \@maketitle
      \else
        \twocolumn[\@maketitle]%
      \fi
    \else
       \newpage
      \global\@topnum\z@   % Prevents figures from going at top of page.
      \@maketitle
    \fi
    \thispagestyle{empty}\@thanks
  \endgroup
  \setcounter{footnote}{0}%
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\@title\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
  \global\let\aipauthors\relax
  \global\let\aipaffil\relax
  \global\let\aunotes\relax
}
\def\@maketitle{%
   \newpage
  \null
  \vskip 4.5em%
  \begin{center}%
  \let \footnote \thanks
    {\LARGE\bfseries \@title \par}%
    \vskip17.5pt%
    {\aipauthors\par\vskip18pt}%
    {\aipaffil\par}%
    {\print@eaddress\par}%
    \vskip 1em%
  \end{center}%
  \par\vskip.6em%
  \ifx\aunotes\@empty\else\footnotetext{\aunotes}\fi%
  \check@authorcount%
}
\fi
\setcounter{secnumdepth}{0}
\newcounter {part}
\newcounter {section}
\newcounter {subsection}[section]
\newcounter {subsubsection}[subsection]
\newcounter {paragraph}[subsubsection]
\newcounter {subparagraph}[paragraph]
\renewcommand \thepart {\@Roman\c@part}
\renewcommand \thesection {\@arabic\c@section}
\renewcommand\thesubsection   {\thesection.\@arabic\c@subsection}
\renewcommand\thesubsubsection{\thesubsection.\@arabic\c@subsubsection}
\renewcommand\theparagraph    {\thesubsubsection.\@arabic\c@paragraph}
\renewcommand\thesubparagraph {\theparagraph.\@arabic\c@subparagraph}
\newcommand\part{%
   \if@noskipsec \leavevmode \fi
   \par
   \addvspace{4ex}%
   \@afterindentfalse
   \secdef\@part\@spart}

\def\@part[#1]#2{%
    \ifnum \c@secnumdepth >\m@ne
      \refstepcounter{part}%
      \addcontentsline{toc}{part}{\thepart\hspace{1em}#1}%
    \else
      \addcontentsline{toc}{part}{#1}%
    \fi
    {\parindent \z@ \raggedright
     \interlinepenalty \@M
     \normalfont
     \ifnum \c@secnumdepth >\m@ne
       \Large\bfseries \partname\nobreakspace\thepart
       \par\nobreak
     \fi
     \huge \bfseries #2%
     \markboth{}{}\par}%
    \nobreak
    \vskip 3ex
    \@afterheading}
\def\@spart#1{%
    {\parindent \z@ \raggedright
     \interlinepenalty \@M
     \normalfont
     \huge \bfseries #1\par}%
     \nobreak
     \vskip 3ex
     \@afterheading}
\newcommand\section{\@startsection {section}{1}{\z@}%
                                   {-16pt \@plus -1ex \@minus -.2ex}%
                                   {11.5pt \@plus.2ex}%
                                   {\normalfont\large\bfseries\centering}}
\newcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                     {-12pt \@plus -1ex \@minus -.2ex}%
                                     {6pt \@plus .2ex}%
                                     {\normalfont\large\bfseries\centering}}
\newcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                     {-12pt \@plus -1ex \@minus -.2ex}%
                                     {6pt \@plus .2ex}%
                                     {\normalfont\normalsize\itshape\centering}}
\newcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                                    {12pt \@plus1ex \@minus.2ex}%
                                    {-1em}%
                                    {\normalfont\normalsize\bfseries}}
\newcommand\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
                                       {3.25ex \@plus1ex \@minus .2ex}%
                                       {-1em}%
                                      {\normalfont\normalsize\bfseries}}
\if@twocolumn
  \setlength\leftmargini  {2em}
\else
  \setlength\leftmargini  {2.5em}
\fi
\leftmargin  \leftmargini
\setlength\leftmarginii  {2.2em}
\setlength\leftmarginiii {1.87em}
\setlength\leftmarginiv  {1.7em}
\if@twocolumn
  \setlength\leftmarginv  {.5em}
  \setlength\leftmarginvi {.5em}
\else
  \setlength\leftmarginv  {1em}
  \setlength\leftmarginvi {1em}
\fi
\setlength  \labelsep  {.5em}
\setlength  \labelwidth{\leftmargini}
\addtolength\labelwidth{-\labelsep}
\@beginparpenalty -\@lowpenalty
\@endparpenalty   -\@lowpenalty
\@itempenalty     -\@lowpenalty
\renewcommand\theenumi{\@arabic\c@enumi}
\renewcommand\theenumii{\@alph\c@enumii}
\renewcommand\theenumiii{\@roman\c@enumiii}
\renewcommand\theenumiv{\@Alph\c@enumiv}
\newcommand\labelenumi{\theenumi.}
\newcommand\labelenumii{(\theenumii)}
\newcommand\labelenumiii{\theenumiii.}
\newcommand\labelenumiv{\theenumiv.}
\renewcommand\p@enumii{\theenumi}
\renewcommand\p@enumiii{\theenumi(\theenumii)}
\renewcommand\p@enumiv{\p@enumiii\theenumiii}
\newcommand\labelitemi{\textbullet}
\newcommand\labelitemii{\ensuremath{\circ}}%\normalfont\bfseries\textendash}
\newcommand\labelitemiii{\textasteriskcentered}
\newcommand\labelitemiv{\textperiodcentered}
\newenvironment{description}
               {\list{}{\labelwidth\z@ \itemindent-\leftmargin
                        \let\makelabel\descriptionlabel}}
               {\endlist}
\newcommand*\descriptionlabel[1]{\hspace\labelsep
                                \normalfont\bfseries #1}
\if@titlepage
  \newenvironment{abstract}{%
      \titlepage
      \null\vfil
      \@beginparpenalty\@lowpenalty
      \begin{center}%
        \bfseries \abstractname
        \@endparpenalty\@M
      \end{center}}%
     {\par\vfil\null\endtitlepage}
\else
  \newenvironment{abstract}{%
      \if@twocolumn
        \section*{\abstractname}%
      \else
        \abstractfont{\noindent\bfseries \abstractname.}%
      \fi}
      {\par}
\fi
\newenvironment{verse}
               {\let\\\@centercr
                \list{}{\itemsep      \z@
                        \itemindent   -1.5em%
                        \listparindent\itemindent
                        \rightmargin  \leftmargin
                        \advance\leftmargin 1.5em}%
                \item\relax}
               {\endlist}
\newenvironment{quotation}
               {\list{}{\listparindent 1.5em%
                        \itemindent    \listparindent
                        \rightmargin   \leftmargin
                        \parsep        \z@ \@plus\p@}%
                \item\relax}
               {\endlist}
\newenvironment{quote}
               {\list{}{\rightmargin\leftmargin}%
                \item\relax}
               {\endlist}
\if@compatibility
\newenvironment{titlepage}
    {%
      \if@twocolumn
        \@restonecoltrue\onecolumn
      \else
        \@restonecolfalse\newpage
      \fi
      \thispagestyle{empty}%
      \setcounter{page}\z@
    }%
    {\if@restonecol\twocolumn \else \newpage \fi
    }
\else
\newenvironment{titlepage}
    {%
      \if@twocolumn
        \@restonecoltrue\onecolumn
      \else
        \@restonecolfalse\newpage
      \fi
      \thispagestyle{empty}%
      \setcounter{page}\@ne
    }%
    {\if@restonecol\twocolumn \else \newpage \fi
     \if@twoside\else
        \setcounter{page}\@ne
     \fi
    }
\fi
\newcommand\appendix{\par
  \setcounter{section}{0}%
  \setcounter{subsection}{0}%
  \gdef\thesection{\@Alph\c@section}}
\setlength\arraycolsep{5\p@}
\setlength\tabcolsep{6\p@}
\setlength\arrayrulewidth{.4\p@}
\setlength\doublerulesep{2\p@}
\setlength\tabbingsep{\labelsep}
\skip\@mpfootins = \skip\footins
\setlength\fboxsep{3\p@}
\setlength\fboxrule{.4\p@}
\renewcommand \theequation {\@arabic\c@equation}
\newcounter{figure}
\renewcommand \thefigure {\@arabic\c@figure}
\def\fps@figure{tbp}
\def\ftype@figure{1}
\def\ext@figure{lof}
\def\fnum@figure{\figurename\nobreakspace\thefigure}
\newenvironment{figure}
               {\@float{figure}}
               {\end@float}
\newenvironment{figure*}
               {\@dblfloat{figure}}
               {\end@dblfloat}
\newcounter{table}
\renewcommand\thetable{\@arabic\c@table}
\def\fps@table{tbp}
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{\tablename\nobreakspace\thetable}
\newenvironment{table}
               {\@float{table}}
               {\end@float}
\newenvironment{table*}
               {\@dblfloat{table}}
               {\end@dblfloat}
\AtBeginDocument{\@ifpackageloaded{rotating}{%
   \renewenvironment{sidewaystable}{\@rotfloat{table}\process@tablenotes}{\tabnote\PrintTableNotes\endtabnote\end@rotfloat}%
   \renewenvironment{sidewaysfigure}{\@rotfloat{figure}}{\end@rotfloat}%
}{}}
\newlength\abovecaptionskip
\newlength\belowcaptionskip
\setlength\abovecaptionskip{10\p@}
\setlength\belowcaptionskip{0\p@}
\def\FigName{figure}
\long\def\@makecaption#1#2{%
  \ifx\FigName\@captype\vskip\abovecaptionskip\fi%
  \sbox\@tempboxa{\captionfont{\bfseries#1.} #2}%
  \ifdim \wd\@tempboxa >\hsize
    {\captionfont{\bfseries#1.} #2\par}%
  \else
    \global \@minipagefalse
    \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
  \fi
  \ifx\FigName\@captype\else\vskip\belowcaptionskip\fi}
\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*\cal{\@fontswitch\relax\mathcal}
\DeclareRobustCommand*\mit{\@fontswitch\relax\mathnormal}
\newcommand\@pnumwidth{1.55em}
\newcommand\@tocrmarg{2.55em}
\newcommand\@dotsep{4.5}
\setcounter{tocdepth}{3}
\newcommand\tableofcontents{%
    \section*{\contentsname
        \@mkboth{%
           \MakeUppercase\contentsname}{\MakeUppercase\contentsname}}%
    \@starttoc{toc}%
    }
\newcommand*\l@part[2]{%
  \ifnum \c@tocdepth >-2\relax
    \addpenalty\@secpenalty
    \addvspace{2.25em \@plus\p@}%
    \setlength\@tempdima{3em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      {\leavevmode
       \large \bfseries #1\hfil \hb@xt@\@pnumwidth{\hss #2}}\par
       \nobreak
       \if@compatibility
         \global\@nobreaktrue
         \everypar{\global\@nobreakfalse\everypar{}}%
      \fi
    \endgroup
  \fi}
\newcommand*\l@section[2]{%
  \ifnum \c@tocdepth >\z@
    \addpenalty\@secpenalty
    \addvspace{1.0em \@plus\p@}%
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \bfseries
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      #1\nobreak\hfil \nobreak\hb@xt@\@pnumwidth{\hss #2}\par
    \endgroup
  \fi}
\newcommand*\l@subsection{\@dottedtocline{2}{1.5em}{2.3em}}
\newcommand*\l@subsubsection{\@dottedtocline{3}{3.8em}{3.2em}}
\newcommand*\l@paragraph{\@dottedtocline{4}{7.0em}{4.1em}}
\newcommand*\l@subparagraph{\@dottedtocline{5}{10em}{5em}}
\newcommand\listoffigures{%
    \section*{\listfigurename}%
      \@mkboth{\MakeUppercase\listfigurename}%
              {\MakeUppercase\listfigurename}%
    \@starttoc{lof}%
    }
\newcommand*\l@figure{\@dottedtocline{1}{1.5em}{2.3em}}
\newcommand\listoftables{%
    \section*{\listtablename}%
      \@mkboth{%
          \MakeUppercase\listtablename}%
         {\MakeUppercase\listtablename}%
    \@starttoc{lot}%
    }
\let\l@table\l@figure
\newdimen\bibindent
\setlength\bibindent{1.5em}
\newenvironment{thebibliography}[1]
     {\section*{\refname}%
      \@mkboth{\MakeUppercase\refname}{\MakeUppercase\refname}%
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \sloppy
      \clubpenalty4000
      \@clubpenalty \clubpenalty
      \widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}
\newcommand\newblock{\hskip .11em\@plus.33em\@minus.07em}
\let\@openbib@code\@empty
\newenvironment{theindex}
               {\if@twocolumn
                  \@restonecolfalse
                \else
                  \@restonecoltrue
                \fi
                \twocolumn[\section*{\indexname}]%
                \@mkboth{\MakeUppercase\indexname}%
                        {\MakeUppercase\indexname}%
                \thispagestyle{plain}\parindent\z@
                \parskip\z@ \@plus .3\p@\relax
                \columnseprule \z@
                \columnsep 35\p@
                \let\item\@idxitem}
               {\if@restonecol\onecolumn\else\clearpage\fi}
\newcommand\@idxitem{\par\hangindent 40\p@}
\newcommand\subitem{\@idxitem \hspace*{20\p@}}
\newcommand\subsubitem{\@idxitem \hspace*{30\p@}}
\newcommand\indexspace{\par \vskip 10\p@ \@plus5\p@ \@minus3\p@\relax}
\renewcommand\footnoterule{%
  \kern-3\p@
  \hrule\@width.4\columnwidth
  \kern2.6\p@}
\newcommand\@makefntext[1]{%
    \parindent 1em%
    \noindent
    \hb@xt@1.8em{\hss\@makefnmark}#1}
\newcommand\contentsname{Contents}
\newcommand\listfigurename{List of Figures}
\newcommand\listtablename{List of Tables}
\newcommand\refname{REFERENCES}
\newcommand\indexname{Index}
\newcommand\figurename{FIGURE}
\newcommand\tablename{TABLE}
\newcommand\partname{Part}
\newcommand\appendixname{Appendix}
\newcommand\abstractname{Abstract}
\def\today{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space\number\day, \number\year}
\setlength\columnsep{10\p@}
\setlength\columnseprule{0\p@}
%%%%%=========
\def\onecolumn{%
%%   \clearpage
  \global\columnwidth\textwidth
  \global\hsize\columnwidth
  \global\linewidth\columnwidth
  \global\@twocolumnfalse
  \col@number \@ne
  \@floatplacement}
%%%========
\newdimen\float@captionwidth%
\newcommand\float@alignment{\centering}
\newcommand\tablefont{\normalfont}%
\newcommand\tableheadfont{\normalfont\bfseries}
\newcommand\removespaces{\unskip\loop\unskip\ifdim\lastskip>\z@\repeat}
\newcommand\process@tablenotes{%
    \def\tablenote{\@ifnextchar[{\@@tablenote}{\@tablenote}}
    \gdef\PrintTableNotes{}%
}

\newbox\float@box
\renewenvironment{table}[1][t]
   {\gdef\float@pos{#1}%
   \process@tablenotes%
   \global \setbox\float@box%
    \color@hbox
      \hbox \bgroup
      \@floatboxreset
         \def\caption##1{\gdef\float@caption{##1}}%
         \def\label##1{\gdef\float@label{##1}}%
         \global\let\float@caption\@undefined
         \global\let\float@label\@undefined
       \normalcolor
       \tablefont
       \ignorespaces
   }
   {%
       \removespaces%
       \outer@nobreak
      \egroup
    \color@endbox
    \setlength\dimen@{\textwidth}%
    \addtolength\dimen@{6pt}%
%%     \ifdim\wd\float@box >\dimen@%  This is for auto-rotation of table
%%       \def\@captype{table}%
%%       \sbox\@tempboxa{\Make@table@body}%
%%       \setlength\dimen@{\ht\@tempboxa}%
%%       \addtolength\dimen@{\dp\@tempboxa}%
%%       \def\know@float@pos{\@float{table}}%
%%       \know@float@pos
%%         \centerline{\rotatebox{90}{\box\@tempboxa}}%
%%       \end@float
%%     \else%
      \def\know@float@pos{\@float{table}}%
      \expandafter\know@float@pos\expandafter[\float@pos]%
        \Make@table@body
      \end@float
%%     \fi%
   }

\newcommand\Make@table@body{%
   \float@alignment% alignment of the table left/center
   \@tempdima\wd\float@box
   \ifdim\@tempdima<\float@captionwidth
     \@tempdima\float@captionwidth
   \fi
   \begin{minipage}\@tempdima
      \ifx\float@caption\@undefined\else
         \caption{\leftskip6pt\float@caption
                   \ifx\float@label\@undefined\else
                     \label\float@label
                   \fi}%
      \fi\par
      \offinterlineskip%
      \vbox{\hsize\wd\float@box
            \box\float@box
           }%\AIP@typeset@source}%
      \PrintTableNotes% Printing the table notes
     \end{minipage}%
}


\renewenvironment{figure}[1][t]
   {\gdef\float@pos{#1}%
    \global \setbox\float@box
    \color@hbox
      \hbox \bgroup
      \@floatboxreset
         \def\caption##1{\gdef\float@caption{##1}}%
         \def\label##1{\gdef\float@label{##1}}%
         \global\let\float@caption\@undefined
         \global\let\float@label\@undefined
       \normalcolor
       \normalfont
       \normalsize
       \ignorespaces
   }
   {%
       \removespaces%
       \outer@nobreak
      \egroup
    \color@endbox
%%     \setlength\dimen@{\textwidth}%
%%     \ifdim\wd\float@box >\dimen@
%%     \def\know@float@pos{\@dblfloat{figure}}%
%%       \know@float@pos%
%%         \centerline{\rotatebox{90}{\Make@figure@body}}%
%%       \end@dblfloat
%%     \else
      \def\know@float@pos{\@float{figure}}%
      \expandafter\know@float@pos\expandafter[\float@pos]%
        \Make@figure@body
      \end@float
%%     \fi
}

\newcommand\Make@figure@body{%
      \float@alignment% alignment of the table left/center
      \setlength\@tempdima{\wd\float@box}%
      \ifdim\@tempdima>\columnwidth
         \@tempdima\textwidth
      \else
         \@tempdima\columnwidth
      \fi
      \begin{minipage}\@tempdima
        \centerline{\vbox{\hsize\wd\float@box
                          \box\float@box
                          }}%\AIP@typeset@source
        \ifx\float@caption\@undefined\else
          \caption{\float@caption
                   \ifx\float@label\@undefined\else
                     \label\float@label
                   \fi}%
        \fi
        \par
     \end{minipage}}
%
\gdef\@makecol{% For putting footnote below the bottom floats
    \setbox\@outputbox \box\@cclv
    \@combinefloats
   \ifvoid\footins
%%       \setbox\@outputbox \box\@cclv
   \else
     \setbox\@outputbox \vbox {%
       \boxmaxdepth \@maxdepth
       \@tempdima\dp\@outputbox%\@cclv
       \unvbox \@outputbox%\@cclv
       \vskip \skip\footins
       \color@begingroup
         \normalcolor
         \footnoterule
         \unvbox \footins
       \color@endgroup
       }%
   \fi
   \let\@elt\relax
   \xdef\@freelist{\@freelist\@midlist}%
   \global \let \@midlist \@empty
%%    \@combinefloats
   \ifvbox\@kludgeins
     \@makespecialcolbox
   \else
     \setbox\@outputbox \vbox to\@colht {%
       \@texttop
       \dimen@ \dp\@outputbox
       \unvbox \@outputbox
       \vskip -\dimen@
       \@textbottom
       }%
   \fi
   \global \maxdepth \@maxdepth
}

%% ============================
\pagestyle{empty}
\pagenumbering{arabic}
\sloppy
\frenchspacing
\flushbottom
%% ============================
\endinput
%%
%% End of file `aip-cp.cls'.
