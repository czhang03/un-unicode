module Symbols (symbolMap, combMap, convertSymbol, convertComb) where

import Data.Map (Map, fromList, (!?))

-- | start of latex symbol, i.e. backslash
latexPrefix :: String
latexPrefix = "\\"

-- | Convert a unicode symbol to latex command
-- 
-- the symbol is surrounded by spaces to make sure 
-- that they are not connected with other symbols
-- We assume the input symbol is a known unicode symbol.
convertSymbol :: Char -> String
convertSymbol symbol =
  case symbolMap !? symbol of
    Just command -> " " ++ latexPrefix ++ command ++ " "
    Nothing -> error "Internal Error, this is a bug, please consider reporting it: cannot convert the unicode symbol \"" ++ symbol : "\""

-- | Converting combining character to latex command
convertComb ::
  -- | the combining character
  Char ->
  -- | the inner latex command or ascii character etc.
  String ->
  String
convertComb comb inner =
  case combMap !? comb of
    Just command -> latexPrefix ++ command ++ "{" ++ inner ++ "}"
    Nothing -> error "Internal Error, this is a bug, please consider reporting it: cannot convert the combining symbol \" " ++ comb : "\""

-- unicode symbols maps to their name and required packages
symbolMap :: Map Char String
symbolMap =
  fromList
    [ ('¬', "neg"),
      ('±', "pm"),
      ('·', "cdot"),
      ('×', "times"),
      ('ð', "matheth"),
      ('÷', "div"),
      ('Ƶ', "Zbar"),
      ('Α', "upAlpha"),
      ('Β', "upBeta"),
      ('Γ', "upGamma"),
      ('Δ', "upDelta"),
      ('Ε', "upEpsilon"),
      ('Ζ', "upZeta"),
      ('Η', "upEta"),
      ('Θ', "upTheta"),
      ('Ι', "upIota"),
      ('Κ', "upKappa"),
      ('Λ', "upLambda"),
      ('Μ', "upMu"),
      ('Ν', "upNu"),
      ('Ξ', "upXi"),
      ('Ο', "upOmicron"),
      ('Π', "upPi"),
      ('Ρ', "upRho"),
      ('Σ', "upSigma"),
      ('Τ', "upTau"),
      ('Υ', "upUpsilon"),
      ('Φ', "upPhi"),
      ('Χ', "upChi"),
      ('Ψ', "upPsi"),
      ('Ω', "upOmega"),
      ('α', "upalpha"),
      ('β', "upbeta"),
      ('γ', "upgamma"),
      ('δ', "updelta"),
      ('ε', "upepsilon"),
      ('ζ', "upzeta"),
      ('η', "upeta"),
      ('θ', "uptheta"),
      ('ι', "upiota"),
      ('κ', "upkappa"),
      ('λ', "uplambda"),
      ('μ', "upmu"),
      ('ν', "upnu"),
      ('ξ', "upxi"),
      ('ο', "upomicron"),
      ('π', "uppi"),
      ('ρ', "uprho"),
      ('ς', "upvarsigma"),
      ('σ', "upsigma"),
      ('τ', "uptau"),
      ('υ', "upupsilon"),
      ('φ', "upvarphi"),
      ('χ', "upchi"),
      ('ψ', "uppsi"),
      ('ω', "upomega"),
      ('ϑ', "upvartheta"),
      ('ϕ', "upphi"),
      ('ϖ', "upvarpi"),
      ('ϰ', "upvarkappa"),
      ('ϱ', "upvarrho"),
      ('ϴ', "upvarTheta"),
      ('ε', "upvarepsilon"),
      ('϶', "upbackepsilon"),
      ('―', "horizbar"),
      ('‖', "Vert"),
      ('‗', "twolowline"),
      ('†', "dagger"),
      ('‡', "ddagger"),
      ('•', "smblkcircle"),
      ('′', "prime"),
      ('″', "dprime"),
      ('‴', "trprime"),
      ('‵', "backprime"),
      ('‶', "backdprime"),
      ('‷', "backtrprime"),
      ('‸', "caretinsert"),
      ('‼', "Exclam"),
      ('⁀', "tieconcat"),
      ('⁃', "hyphenbullet"),
      ('⁄', "fracslash"),
      ('⁇', "Question"),
      ('⁐', "closure"),
      ('⁗', "qprime"),
      ('€', "euro"),
      ('ℇ', "Eulerconst"),
      ('ℎ', "Planckconst"),
      ('ℏ', "hslash"),
      ('ℑ', "Im"),
      ('ℓ', "ell"),
      ('℘', "wp"),
      ('ℜ', "Re"),
      ('℧', "mho"),
      ('℩', "turnediota"),
      ('Å', "Angstrom"),
      ('Ⅎ', "Finv"),
      ('ℵ', "aleph"),
      ('ℶ', "beth"),
      ('ℷ', "gimel"),
      ('ℸ', "daleth"),
      ('ℼ', "Bbbpi"),
      ('ℽ', "Bbbgamma"),
      ('ℾ', "BbbGamma"),
      ('ℿ', "BbbPi"),
      ('⅀', "Bbbsum"),
      ('⅁', "Game"),
      ('⅂', "sansLturned"),
      ('⅃', "sansLmirrored"),
      ('⅄', "Yup"),
      ('ⅅ', "mitBbbD"),
      ('ⅆ', "mitBbbd"),
      ('ⅇ', "mitBbbe"),
      ('ⅈ', "mitBbbi"),
      ('ⅉ', "mitBbbj"),
      ('⅊', "PropertyLine"),
      ('⅋', "upand"),
      ('←', "leftarrow"),
      ('↑', "uparrow"),
      ('→', "rightarrow"),
      ('↓', "downarrow"),
      ('↔', "leftrightarrow"),
      ('↕', "updownarrow"),
      ('↖', "nwarrow"),
      ('↗', "nearrow"),
      ('↘', "searrow"),
      ('↙', "swarrow"),
      ('↚', "nleftarrow"),
      ('↛', "nrightarrow"),
      ('↜', "leftwavearrow"),
      ('↝', "rightwavearrow"),
      ('↞', "twoheadleftarrow"),
      ('↟', "twoheaduparrow"),
      ('↠', "twoheadrightarrow"),
      ('↡', "twoheaddownarrow"),
      ('↢', "leftarrowtail"),
      ('↣', "rightarrowtail"),
      ('↤', "mapsfrom"),
      ('↥', "mapsup"),
      ('↦', "mapsto"),
      ('↧', "mapsdown"),
      ('↨', "updownarrowbar"),
      ('↩', "hookleftarrow"),
      ('↪', "hookrightarrow"),
      ('↫', "looparrowleft"),
      ('↬', "looparrowright"),
      ('↭', "leftrightsquigarrow"),
      ('↮', "nleftrightarrow"),
      ('↯', "downzigzagarrow"),
      ('↰', "Lsh"),
      ('↱', "Rsh"),
      ('↲', "Ldsh"),
      ('↳', "Rdsh"),
      ('↴', "linefeed"),
      ('↵', "carriagereturn"),
      ('↶', "curvearrowleft"),
      ('↷', "curvearrowright"),
      ('↸', "barovernorthwestarrow"),
      ('↹', "barleftarrowrightarrowbar"),
      ('↺', "acwopencirclearrow"),
      ('↻', "cwopencirclearrow"),
      ('↼', "leftharpoonup"),
      ('↽', "leftharpoondown"),
      ('↾', "upharpoonright"),
      ('↿', "upharpoonleft"),
      ('⇀', "rightharpoonup"),
      ('⇁', "rightharpoondown"),
      ('⇂', "downharpoonright"),
      ('⇃', "downharpoonleft"),
      ('⇄', "rightleftarrows"),
      ('⇅', "updownarrows"),
      ('⇆', "leftrightarrows"),
      ('⇇', "leftleftarrows"),
      ('⇈', "upuparrows"),
      ('⇉', "rightrightarrows"),
      ('⇊', "downdownarrows"),
      ('⇋', "leftrightharpoons"),
      ('⇌', "rightleftharpoons"),
      ('⇍', "nLeftarrow"),
      ('⇎', "nLeftrightarrow"),
      ('⇏', "nRightarrow"),
      ('⇐', "Leftarrow"),
      ('⇑', "Uparrow"),
      ('⇒', "Rightarrow"),
      ('⇓', "Downarrow"),
      ('⇔', "Leftrightarrow"),
      ('⇕', "Updownarrow"),
      ('⇖', "Nwarrow"),
      ('⇗', "Nearrow"),
      ('⇘', "Searrow"),
      ('⇙', "Swarrow"),
      ('⇚', "Lleftarrow"),
      ('⇛', "Rrightarrow"),
      ('⇜', "leftsquigarrow"),
      ('⇝', "rightsquigarrow"),
      ('⇞', "nHuparrow"),
      ('⇟', "nHdownarrow"),
      ('⇠', "leftdasharrow"),
      ('⇡', "updasharrow"),
      ('⇢', "rightdasharrow"),
      ('⇣', "downdasharrow"),
      ('⇤', "barleftarrow"),
      ('⇥', "rightarrowbar"),
      ('⇦', "leftwhitearrow"),
      ('⇧', "upwhitearrow"),
      ('⇨', "rightwhitearrow"),
      ('⇩', "downwhitearrow"),
      ('⇪', "whitearrowupfrombar"),
      ('⇴', "circleonrightarrow"),
      ('⇵', "downuparrows"),
      ('⇶', "rightthreearrows"),
      ('⇷', "nvleftarrow"),
      ('⇸', "nvrightarrow"),
      ('⇹', "nvleftrightarrow"),
      ('⇺', "nVleftarrow"),
      ('⇻', "nVrightarrow"),
      ('⇼', "nVleftrightarrow"),
      ('⇽', "leftarrowtriangle"),
      ('⇾', "rightarrowtriangle"),
      ('⇿', "leftrightarrowtriangle"),
      ('∀', "forall"),
      ('∁', "complement"),
      ('∂', "partial"),
      ('∃', "exists"),
      ('∄', "nexists"),
      ('∅', "varnothing"),
      ('∆', "increment"),
      ('∇', "nabla"),
      ('∈', "in"),
      ('∉', "notin"),
      ('∊', "smallin"),
      ('∋', "ni"),
      ('∌', "nni"),
      ('∍', "smallni"),
      ('∎', "QED"),
      ('∏', "prod"),
      ('∐', "coprod"),
      ('∑', "sum"),
      ('−', "minus"),
      ('∓', "mp"),
      ('∔', "dotplus"),
      ('∕', "divslash"),
      ('∖', "smallsetminus"),
      ('∗', "ast"),
      ('∘', "vysmwhtcircle"),
      ('∙', "vysmblkcircle"),
      ('∝', "propto"),
      ('∞', "infty"),
      ('∟', "rightangle"),
      ('∠', "angle"),
      ('∡', "measuredangle"),
      ('∢', "sphericalangle"),
      ('∣', "mid"),
      ('∤', "nmid"),
      ('∥', "parallel"),
      ('∦', "nparallel"),
      ('∧', "wedge"),
      ('∨', "vee"),
      ('∩', "cap"),
      ('∪', "cup"),
      ('∫', "int"),
      ('∬', "iint"),
      ('∭', "iiint"),
      ('∮', "oint"),
      ('∯', "oiint"),
      ('∰', "oiiint"),
      ('∱', "intclockwise"),
      ('∲', "varointclockwise"),
      ('∳', "ointctrclockwise"),
      ('∴', "therefore"),
      ('∵', "because"),
      ('∶', "mathratio"),
      ('∷', "Colon"),
      ('∸', "dotminus"),
      ('∹', "dashcolon"),
      ('∺', "dotsminusdots"),
      ('∻', "kernelcontraction"),
      ('∼', "sim"),
      ('∽', "backsim"),
      ('∾', "invlazys"),
      ('∿', "sinewave"),
      ('≀', "wr"),
      ('≁', "nsim"),
      ('≂', "eqsim"),
      ('≃', "simeq"),
      ('≄', "nsime"),
      ('≅', "cong"),
      ('≆', "simneqq"),
      ('≇', "ncong"),
      ('≈', "approx"),
      ('≉', "napprox"),
      ('≊', "approxeq"),
      ('≋', "approxident"),
      ('≌', "backcong"),
      ('≍', "asymp"),
      ('≎', "Bumpeq"),
      ('≏', "bumpeq"),
      ('≐', "doteq"),
      ('≑', "Doteq"),
      ('≒', "fallingdotseq"),
      ('≓', "risingdotseq"),
      ('≔', "coloneq"),
      ('≕', "eqcolon"),
      ('≖', "eqcirc"),
      ('≗', "circeq"),
      ('≘', "arceq"),
      ('≙', "wedgeq"),
      ('≚', "veeeq"),
      ('≛', "stareq"),
      ('≜', "triangleq"),
      ('≝', "eqdef"),
      ('≞', "measeq"),
      ('≟', "questeq"),
      ('≠', "ne"),
      ('≡', "equiv"),
      ('≢', "nequiv"),
      ('≣', "Equiv"),
      ('≤', "leq"),
      ('≥', "geq"),
      ('≦', "leqq"),
      ('≧', "geqq"),
      ('≨', "lneqq"),
      ('≩', "gneqq"),
      ('≪', "ll"),
      ('≫', "gg"),
      ('≬', "between"),
      ('≭', "nasymp"),
      ('≮', "nless"),
      ('≯', "ngtr"),
      ('≰', "nleq"),
      ('≱', "ngeq"),
      ('≲', "lesssim"),
      ('≳', "gtrsim"),
      ('≴', "nlesssim"),
      ('≵', "ngtrsim"),
      ('≶', "lessgtr"),
      ('≷', "gtrless"),
      ('≸', "nlessgtr"),
      ('≹', "ngtrless"),
      ('≺', "prec"),
      ('≻', "succ"),
      ('≼', "preccurlyeq"),
      ('≽', "succcurlyeq"),
      ('≾', "precsim"),
      ('≿', "succsim"),
      ('⊀', "nprec"),
      ('⊁', "nsucc"),
      ('⊂', "subset"),
      ('⊃', "supset"),
      ('⊄', "nsubset"),
      ('⊅', "nsupset"),
      ('⊆', "subseteq"),
      ('⊇', "supseteq"),
      ('⊈', "nsubseteq"),
      ('⊉', "nsupseteq"),
      ('⊊', "subsetneq"),
      ('⊋', "supsetneq"),
      ('⊌', "cupleftarrow"),
      ('⊍', "cupdot"),
      ('⊎', "uplus"),
      ('⊏', "sqsubset"),
      ('⊐', "sqsupset"),
      ('⊑', "sqsubseteq"),
      ('⊒', "sqsupseteq"),
      ('⊓', "sqcap"),
      ('⊔', "sqcup"),
      ('⊕', "oplus"),
      ('⊖', "ominus"),
      ('⊗', "otimes"),
      ('⊘', "oslash"),
      ('⊙', "odot"),
      ('⊚', "circledcirc"),
      ('⊛', "circledast"),
      ('⊜', "circledequal"),
      ('⊝', "circleddash"),
      ('⊞', "boxplus"),
      ('⊟', "boxminus"),
      ('⊠', "boxtimes"),
      ('⊡', "boxdot"),
      ('⊢', "vdash"),
      ('⊣', "dashv"),
      ('⊤', "top"),
      ('⊥', "bot"),
      ('⊦', "assert"),
      ('⊧', "models"),
      ('⊨', "vDash"),
      ('⊩', "Vdash"),
      ('⊪', "Vvdash"),
      ('⊫', "VDash"),
      ('⊬', "nvdash"),
      ('⊭', "nvDash"),
      ('⊮', "nVdash"),
      ('⊯', "nVDash"),
      ('⊰', "prurel"),
      ('⊱', "scurel"),
      ('⊲', "vartriangleleft"),
      ('⊳', "vartriangleright"),
      ('⊴', "trianglelefteq"),
      ('⊵', "trianglerighteq"),
      ('⊶', "origof"),
      ('⊷', "imageof"),
      ('⊸', "multimap"),
      ('⊹', "hermitmatrix"),
      ('⊺', "intercal"),
      ('⊻', "veebar"),
      ('⊼', "barwedge"),
      ('⊽', "barvee"),
      ('⊾', "measuredrightangle"),
      ('⊿', "varlrtriangle"),
      ('⋀', "bigwedge"),
      ('⋁', "bigvee"),
      ('⋂', "bigcap"),
      ('⋃', "bigcup"),
      ('⋄', "smwhtdiamond"),
      ('⋅', "cdot"),
      ('⋆', "star"),
      ('⋇', "divideontimes"),
      ('⋈', "bowtie"),
      ('⋉', "ltimes"),
      ('⋊', "rtimes"),
      ('⋋', "leftthreetimes"),
      ('⋌', "rightthreetimes"),
      ('⋍', "backsimeq"),
      ('⋎', "curlyvee"),
      ('⋏', "curlywedge"),
      ('⋐', "Subset"),
      ('⋑', "Supset"),
      ('⋒', "Cap"),
      ('⋓', "Cup"),
      ('⋔', "pitchfork"),
      ('⋕', "equalparallel"),
      ('⋖', "lessdot"),
      ('⋗', "gtrdot"),
      ('⋘', "lll"),
      ('⋙', "ggg"),
      ('⋚', "lesseqgtr"),
      ('⋛', "gtreqless"),
      ('⋜', "eqless"),
      ('⋝', "eqgtr"),
      ('⋞', "curlyeqprec"),
      ('⋟', "curlyeqsucc"),
      ('⋠', "npreccurlyeq"),
      ('⋡', "nsucccurlyeq"),
      ('⋢', "nsqsubseteq"),
      ('⋣', "nsqsupseteq"),
      ('⋤', "sqsubsetneq"),
      ('⋥', "sqsupsetneq"),
      ('⋦', "lnsim"),
      ('⋧', "gnsim"),
      ('⋨', "precnsim"),
      ('⋩', "succnsim"),
      ('⋪', "nvartriangleleft"),
      ('⋫', "nvartriangleright"),
      ('⋬', "ntrianglelefteq"),
      ('⋭', "ntrianglerighteq"),
      ('‥', "enleadertwodots"),
      ('…', "ldots"),
      ('⋮', "vdots"),
      ('⋯', "cdots"),
      ('⋰', "adots"),
      ('⋱', "ddots"),
      ('⋲', "disin"),
      ('⋳', "varisins"),
      ('⋴', "isins"),
      ('⋵', "isindot"),
      ('⋶', "varisinobar"),
      ('⋷', "isinobar"),
      ('⋸', "isinvb"),
      ('⋹', "isinE"),
      ('⋺', "nisd"),
      ('⋻', "varnis"),
      ('⋼', "nis"),
      ('⋽', "varniobar"),
      ('⋾', "niobar"),
      ('⋿', "bagmember"),
      ('⌀', "diameter"),
      ('⌂', "house"),
      ('⌅', "varbarwedge"),
      ('⌆', "vardoublebarwedge"),
      ('⌈', "lceil"),
      ('⌉', "rceil"),
      ('⌊', "lfloor"),
      ('⌋', "rfloor"),
      ('⌐', "invnot"),
      ('⌑', "sqlozenge"),
      ('⌒', "profline"),
      ('⌓', "profsurf"),
      ('⌗', "viewdata"),
      ('⌙', "turnednot"),
      ('⌜', "ulcorner"),
      ('⌝', "urcorner"),
      ('⌞', "llcorner"),
      ('⌟', "lrcorner"),
      ('⌠', "inttop"),
      ('⌡', "intbottom"),
      ('⌢', "frown"),
      ('⌣', "smile"),
      ('⌬', "varhexagonlrbonds"),
      ('⌲', "conictaper"),
      ('⌶', "topbot"),
      ('⌽', "obar"),
      ('⌿', "APLnotslash"),
      ('⍀', "APLnotbackslash"),
      ('⍓', "APLboxupcaret"),
      ('⍰', "APLboxquestion"),
      ('⍼', "rangledownzigzagarrow"),
      ('⎔', "hexagon"),
      ('⟦', "llbracket"),
      ('⟧', "rrbracket"),
      ('⟦', "llparenthesis"),
      ('⟧', "rrparenthesis "),
      ('⎛', "lparenuend"),
      ('⎜', "lparenextender"),
      ('⎝', "lparenlend"),
      ('⎞', "rparenuend"),
      ('⎟', "rparenextender"),
      ('⎠', "rparenlend"),
      ('⎡', "lbrackuend"),
      ('⎢', "lbrackextender"),
      ('⎣', "lbracklend"),
      ('⎤', "rbrackuend"),
      ('⎥', "rbrackextender"),
      ('⎦', "rbracklend"),
      ('⎧', "lbraceuend"),
      ('⎨', "lbracemid"),
      ('⎩', "lbracelend"),
      ('⎪', "vbraceextender"),
      ('⎫', "rbraceuend"),
      ('⎬', "rbracemid"),
      ('⎭', "rbracelend"),
      ('⎮', "intextender"),
      ('⎯', "harrowextender"),
      ('⎰', "lmoustache"),
      ('⎱', "rmoustache"),
      ('⎲', "sumtop"),
      ('⎳', "sumbottom"),
      ('⎶', "bbrktbrk"),
      ('⎷', "sqrtbottom"),
      ('⎸', "lvboxline"),
      ('⎹', "rvboxline"),
      ('⏎', "varcarriagereturn"),
      ('⏢', "trapezium"),
      ('⏣', "benzenr"),
      ('⏤', "strns"),
      ('⏥', "fltns"),
      ('⏦', "accurrent"),
      ('⏧', "elinters"),
      ('␢', "blanksymbol"),
      ('␣', "mathvisiblespace"),
      ('┆', "bdtriplevdash"),
      ('▀', "blockuphalf"),
      ('▄', "blocklowhalf"),
      ('█', "blockfull"),
      ('▌', "blocklefthalf"),
      ('▐', "blockrighthalf"),
      ('░', "blockqtrshaded"),
      ('▒', "blockhalfshaded"),
      ('▓', "blockthreeqtrshaded"),
      ('■', "mdlgblksquare"),
      ('□', "mdlgwhtsquare"),
      ('▢', "squoval"),
      ('▣', "blackinwhitesquare"),
      ('▤', "squarehfill"),
      ('▥', "squarevfill"),
      ('▦', "squarehvfill"),
      ('▧', "squarenwsefill"),
      ('▨', "squareneswfill"),
      ('▩', "squarecrossfill"),
      ('▪', "smblksquare"),
      ('▫', "smwhtsquare"),
      ('▬', "hrectangleblack"),
      ('▭', "hrectangle"),
      ('▮', "vrectangleblack"),
      ('▯', "vrectangle"),
      ('▰', "parallelogramblack"),
      ('▱', "parallelogram"),
      ('▲', "bigblacktriangleup"),
      ('△', "bigtriangleup"),
      ('▴', "blacktriangle"),
      ('▵', "vartriangle"),
      ('▶', "blacktriangleright"),
      ('▷', "triangleright"),
      ('▸', "smallblacktriangleright"),
      ('▹', "smalltriangleright"),
      ('►', "blackpointerright"),
      ('▻', "whitepointerright"),
      ('▼', "bigblacktriangledown"),
      ('▽', "bigtriangledown"),
      ('▾', "blacktriangledown"),
      ('▿', "triangledown"),
      ('◀', "blacktriangleleft"),
      ('◁', "triangleleft"),
      ('◂', "smallblacktriangleleft"),
      ('◃', "smalltriangleleft"),
      ('◄', "blackpointerleft"),
      ('◅', "whitepointerleft"),
      ('◆', "mdlgblkdiamond"),
      ('◇', "mdlgwhtdiamond"),
      ('◈', "blackinwhitediamond"),
      ('◉', "fisheye"),
      ('◊', "mdlgwhtlozenge"),
      ('○', "mdlgwhtcircle"),
      ('◌', "dottedcircle"),
      ('◍', "circlevertfill"),
      ('◎', "bullseye"),
      ('●', "mdlgblkcircle"),
      ('◐', "circlelefthalfblack"),
      ('◑', "circlerighthalfblack"),
      ('◒', "circlebottomhalfblack"),
      ('◓', "circletophalfblack"),
      ('◔', "circleurquadblack"),
      ('◕', "blackcircleulquadwhite"),
      ('◖', "blacklefthalfcircle"),
      ('◗', "blackrighthalfcircle"),
      ('◘', "inversebullet"),
      ('◙', "inversewhitecircle"),
      ('◚', "invwhiteupperhalfcircle"),
      ('◛', "invwhitelowerhalfcircle"),
      ('◜', "ularc"),
      ('◝', "urarc"),
      ('◞', "lrarc"),
      ('◟', "llarc"),
      ('◠', "topsemicircle"),
      ('◡', "botsemicircle"),
      ('◢', "lrblacktriangle"),
      ('◣', "llblacktriangle"),
      ('◤', "ulblacktriangle"),
      ('◥', "urblacktriangle"),
      ('◦', "smwhtcircle"),
      ('◧', "squareleftblack"),
      ('◨', "squarerightblack"),
      ('◩', "squareulblack"),
      ('◪', "squarelrblack"),
      ('◫', "boxbar"),
      ('◬', "trianglecdot"),
      ('◭', "triangleleftblack"),
      ('◮', "trianglerightblack"),
      ('◯', "lgwhtcircle"),
      ('◰', "squareulquad"),
      ('◱', "squarellquad"),
      ('◲', "squarelrquad"),
      ('◳', "squareurquad"),
      ('◴', "circleulquad"),
      ('◵', "circlellquad"),
      ('◶', "circlelrquad"),
      ('◷', "circleurquad"),
      ('◸', "ultriangle"),
      ('◹', "urtriangle"),
      ('◺', "lltriangle"),
      ('◻', "mdwhtsquare"),
      ('◼', "mdblksquare"),
      ('◽', "mdsmwhtsquare"),
      ('◾', "mdsmblksquare"),
      ('◿', "lrtriangle"),
      ('★', "bigstar"),
      ('☆', "bigwhitestar"),
      ('☉', "astrosun"),
      ('☡', "danger"),
      ('☻', "blacksmiley"),
      ('☼', "sun"),
      ('☽', "rightmoon"),
      ('☾', "leftmoon"),
      ('♀', "female"),
      ('♂', "male"),
      ('♠', "spadesuit"),
      ('♡', "heartsuit"),
      ('♢', "diamondsuit"),
      ('♣', "clubsuit"),
      ('♤', "varspadesuit"),
      ('♥', "varheartsuit"),
      ('♦', "vardiamondsuit"),
      ('♧', "varclubsuit"),
      ('♩', "quarternote"),
      ('♪', "eighthnote"),
      ('♫', "twonotes"),
      ('♭', "flat"),
      ('♮', "natural"),
      ('♯', "sharp"),
      ('♾', "acidfree"),
      ('⚀', "dicei"),
      ('⚁', "diceii"),
      ('⚂', "diceiii"),
      ('⚃', "diceiv"),
      ('⚄', "dicev"),
      ('⚅', "dicevi"),
      ('⚆', "circledrightdot"),
      ('⚇', "circledtwodots"),
      ('⚈', "blackcircledrightdot"),
      ('⚉', "blackcircledtwodots"),
      ('⚥', "Hermaphrodite"),
      ('⚪', "mdwhtcircle"),
      ('⚫', "mdblkcircle"),
      ('⚬', "mdsmwhtcircle"),
      ('⚲', "neuter"),
      ('✓', "checkmark"),
      ('✠', "maltese"),
      ('✪', "circledstar"),
      ('✶', "varstar"),
      ('✽', "dingasterisk"),
      ('〔', "lbrbrak"),
      ('〕', "rbrbrak"),
      ('➛', "draftingarrow"),
      ('⟀', "threedangle"),
      ('⟁', "whiteinwhitetriangle"),
      ('⟂', "perp"),
      ('⟃', "subsetcirc"),
      ('⟄', "supsetcirc"),
      ('⟅', "lbag"),
      ('⟆', "rbag"),
      ('⟇', "veedot"),
      ('⟈', "bsolhsub"),
      ('⟉', "suphsol"),
      ('⟌', "longdivision"),
      ('⟐', "diamondcdot"),
      ('⟑', "wedgedot"),
      ('⟒', "upin"),
      ('⟓', "pullback"),
      ('⟔', "pushout"),
      ('⟕', "leftouterjoin"),
      ('⟖', "rightouterjoin"),
      ('⟗', "fullouterjoin"),
      ('⟘', "bigbot"),
      ('⟙', "bigtop"),
      ('⟚', "DashVDash"),
      ('⟛', "dashVdash"),
      ('⟜', "multimapinv"),
      ('⟝', "vlongdash"),
      ('⟞', "longdashv"),
      ('⟟', "cirbot"),
      ('⟠', "lozengeminus"),
      ('⟡', "concavediamond"),
      ('⟢', "concavediamondtickleft"),
      ('⟣', "concavediamondtickright"),
      ('⟤', "whitesquaretickleft"),
      ('⟥', "whitesquaretickright"),
      ('⟦', "lBrack"),
      ('⟧', "rBrack"),
      ('⟨', "langle"),
      ('⟩', "rangle"),
      ('⟪', "lAngle"),
      ('⟫', "rAngle"),
      ('〘', "Lbrbrak"),
      ('〙', "Rbrbrak"),
      ('⟮', "lgroup"),
      ('⟯', "rgroup"),
      ('⟰', "UUparrow"),
      ('⟱', "DDownarrow"),
      ('⟲', "acwgapcirclearrow"),
      ('⟳', "cwgapcirclearrow"),
      ('⟴', "rightarrowonoplus"),
      ('⟵', "longleftarrow"),
      ('⟶', "longrightarrow"),
      ('⟷', "longleftrightarrow"),
      ('⟸', "Longleftarrow"),
      ('⟹', "Longrightarrow"),
      ('⟺', "Longleftrightarrow"),
      ('⟻', "longmapsfrom"),
      ('⟼', "longmapsto"),
      ('⟽', "Longmapsfrom"),
      ('⟾', "Longmapsto"),
      ('⟿', "longrightsquigarrow"),
      ('⤀', "nvtwoheadrightarrow"),
      ('⤁', "nVtwoheadrightarrow"),
      ('⤂', "nvLeftarrow"),
      ('⤃', "nvRightarrow"),
      ('⤄', "nvLeftrightarrow"),
      ('⤅', "twoheadmapsto"),
      ('⤆', "Mapsfrom"),
      ('⤇', "Mapsto"),
      ('⤈', "downarrowbarred"),
      ('⤉', "uparrowbarred"),
      ('⤊', "Uuparrow"),
      ('⤋', "Ddownarrow"),
      ('⤌', "leftbkarrow"),
      ('⤍', "rightbkarrow"),
      ('⤎', "leftdbkarrow"),
      ('⤏', "dbkarow"),
      ('⤐', "drbkarow"),
      ('⤑', "rightdotarrow"),
      ('⤒', "baruparrow"),
      ('⤓', "downarrowbar"),
      ('⤔', "nvrightarrowtail"),
      ('⤕', "nVrightarrowtail"),
      ('⤖', "twoheadrightarrowtail"),
      ('⤗', "nvtwoheadrightarrowtail"),
      ('⤘', "nVtwoheadrightarrowtail"),
      ('⤙', "lefttail"),
      ('⤚', "righttail"),
      ('⤛', "leftdbltail"),
      ('⤜', "rightdbltail"),
      ('⤝', "diamondleftarrow"),
      ('⤞', "rightarrowdiamond"),
      ('⤟', "diamondleftarrowbar"),
      ('⤠', "barrightarrowdiamond"),
      ('⤡', "nwsearrow"),
      ('⤢', "neswarrow"),
      ('⤣', "hknwarrow"),
      ('⤤', "hknearrow"),
      ('⤥', "hksearow"),
      ('⤦', "hkswarow"),
      ('⤧', "tona"),
      ('⤨', "toea"),
      ('⤩', "tosa"),
      ('⤪', "towa"),
      ('⤫', "rdiagovfdiag"),
      ('⤬', "fdiagovrdiag"),
      ('⤭', "seovnearrow"),
      ('⤮', "neovsearrow"),
      ('⤯', "fdiagovnearrow"),
      ('⤰', "rdiagovsearrow"),
      ('⤱', "neovnwarrow"),
      ('⤲', "nwovnearrow"),
      ('⤳', "rightcurvedarrow"),
      ('⤴', "uprightcurvearrow"),
      ('⤵', "downrightcurvedarrow"),
      ('⤶', "leftdowncurvedarrow"),
      ('⤷', "rightdowncurvedarrow"),
      ('⤸', "cwrightarcarrow"),
      ('⤹', "acwleftarcarrow"),
      ('⤺', "acwoverarcarrow"),
      ('⤻', "acwunderarcarrow"),
      ('⤼', "curvearrowrightminus"),
      ('⤽', "curvearrowleftplus"),
      ('⤾', "cwundercurvearrow"),
      ('⤿', "ccwundercurvearrow"),
      ('⥀', "acwcirclearrow"),
      ('⥁', "cwcirclearrow"),
      ('⥂', "rightarrowshortleftarrow"),
      ('⥃', "leftarrowshortrightarrow"),
      ('⥄', "shortrightarrowleftarrow"),
      ('⥅', "rightarrowplus"),
      ('⥆', "leftarrowplus"),
      ('⥇', "rightarrowx"),
      ('⥈', "leftrightarrowcircle"),
      ('⥉', "twoheaduparrowcircle"),
      ('⥊', "leftrightharpoonupdown"),
      ('⥋', "leftrightharpoondownup"),
      ('⥌', "updownharpoonrightleft"),
      ('⥍', "updownharpoonleftright"),
      ('⥎', "leftrightharpoonupup"),
      ('⥏', "updownharpoonrightright"),
      ('⥐', "leftrightharpoondowndown"),
      ('⥑', "updownharpoonleftleft"),
      ('⥒', "barleftharpoonup"),
      ('⥓', "rightharpoonupbar"),
      ('⥔', "barupharpoonright"),
      ('⥕', "downharpoonrightbar"),
      ('⥖', "barleftharpoondown"),
      ('⥗', "rightharpoondownbar"),
      ('⥘', "barupharpoonleft"),
      ('⥙', "downharpoonleftbar"),
      ('⥚', "leftharpoonupbar"),
      ('⥛', "barrightharpoonup"),
      ('⥜', "upharpoonrightbar"),
      ('⥝', "bardownharpoonright"),
      ('⥞', "leftharpoondownbar"),
      ('⥟', "barrightharpoondown"),
      ('⥠', "upharpoonleftbar"),
      ('⥡', "bardownharpoonleft"),
      ('⥢', "leftharpoonsupdown"),
      ('⥣', "upharpoonsleftright"),
      ('⥤', "rightharpoonsupdown"),
      ('⥥', "downharpoonsleftright"),
      ('⥦', "leftrightharpoonsup"),
      ('⥧', "leftrightharpoonsdown"),
      ('⥨', "rightleftharpoonsup"),
      ('⥩', "rightleftharpoonsdown"),
      ('⥪', "leftharpoonupdash"),
      ('⥫', "dashleftharpoondown"),
      ('⥬', "rightharpoonupdash"),
      ('⥭', "dashrightharpoondown"),
      ('⥮', "updownharpoonsleftright"),
      ('⥯', "downupharpoonsleftright"),
      ('⥰', "rightimply"),
      ('⥱', "equalrightarrow"),
      ('⥲', "similarrightarrow"),
      ('⥳', "leftarrowsimilar"),
      ('⥴', "rightarrowsimilar"),
      ('⥵', "rightarrowapprox"),
      ('⥶', "ltlarr"),
      ('⥷', "leftarrowless"),
      ('⥸', "gtrarr"),
      ('⥹', "subrarr"),
      ('⥺', "leftarrowsubset"),
      ('⥻', "suplarr"),
      ('⥼', "leftfishtail"),
      ('⥽', "rightfishtail"),
      ('⥾', "upfishtail"),
      ('⥿', "downfishtail"),
      ('⦀', "Vvert"),
      ('⦁', "mdsmblkcircle"),
      ('⦂', "typecolon"),
      ('⦃', "lBrace"),
      ('⦄', "rBrace"),
      ('⦅', "lParen"),
      ('⦆', "rParen"),
      ('⦇', "llparenthesis"),
      ('⦈', "rrparenthesis"),
      ('⦉', "llangle"),
      ('⦊', "rrangle"),
      ('⦋', "lbrackubar"),
      ('⦌', "rbrackubar"),
      ('⦍', "lbrackultick"),
      ('⦎', "rbracklrtick"),
      ('⦏', "lbracklltick"),
      ('⦐', "rbrackurtick"),
      ('⦑', "langledot"),
      ('⦒', "rangledot"),
      ('⦓', "lparenless"),
      ('⦔', "rparengtr"),
      ('⦕', "Lparengtr"),
      ('⦖', "Rparenless"),
      ('⦗', "lblkbrbrak"),
      ('⦘', "rblkbrbrak"),
      ('⦙', "fourvdots"),
      ('⦚', "vzigzag"),
      ('⦛', "measuredangleleft"),
      ('⦜', "rightanglesqr"),
      ('⦝', "rightanglemdot"),
      ('⦞', "angles"),
      ('⦟', "angdnr"),
      ('⦠', "gtlpar"),
      ('⦡', "sphericalangleup"),
      ('⦢', "turnangle"),
      ('⦣', "revangle"),
      ('⦤', "angleubar"),
      ('⦥', "revangleubar"),
      ('⦦', "wideangledown"),
      ('⦧', "wideangleup"),
      ('⦨', "measanglerutone"),
      ('⦩', "measanglelutonw"),
      ('⦪', "measanglerdtose"),
      ('⦫', "measangleldtosw"),
      ('⦬', "measangleurtone"),
      ('⦭', "measangleultonw"),
      ('⦮', "measangledrtose"),
      ('⦯', "measangledltosw"),
      ('⦰', "revemptyset"),
      ('⦱', "emptysetobar"),
      ('⦲', "emptysetocirc"),
      ('⦳', "emptysetoarr"),
      ('⦴', "emptysetoarrl"),
      ('⦵', "circlehbar"),
      ('⦶', "circledvert"),
      ('⦷', "circledparallel"),
      ('⦸', "obslash"),
      ('⦹', "operp"),
      ('⦺', "obot"),
      ('⦻', "olcross"),
      ('⦼', "odotslashdot"),
      ('⦽', "uparrowoncircle"),
      ('⦾', "circledwhitebullet"),
      ('⦿', "circledbullet"),
      ('⧀', "olessthan"),
      ('⧁', "ogreaterthan"),
      ('⧂', "cirscir"),
      ('⧃', "cirE"),
      ('⧄', "boxdiag"),
      ('⧅', "boxbslash"),
      ('⧆', "boxast"),
      ('⧇', "boxcircle"),
      ('⧈', "boxbox"),
      ('⧉', "boxonbox"),
      ('⧊', "triangleodot"),
      ('⧋', "triangleubar"),
      ('⧌', "triangles"),
      ('⧍', "triangleserifs"),
      ('⧎', "rtriltri"),
      ('⧏', "ltrivb"),
      ('⧐', "vbrtri"),
      ('⧑', "lfbowtie"),
      ('⧒', "rfbowtie"),
      ('⧓', "fbowtie"),
      ('⧔', "lftimes"),
      ('⧕', "rftimes"),
      ('⧖', "hourglass"),
      ('⧗', "blackhourglass"),
      ('⧘', "lvzigzag"),
      ('⧙', "rvzigzag"),
      ('⧚', "Lvzigzag"),
      ('⧛', "Rvzigzag"),
      ('⧜', "iinfin"),
      ('⧝', "tieinfty"),
      ('⧞', "nvinfty"),
      ('⧟', "dualmap"),
      ('⧠', "laplac"),
      ('⧡', "lrtriangleeq"),
      ('⧢', "shuffle"),
      ('⧣', "eparsl"),
      ('⧤', "smeparsl"),
      ('⧥', "eqvparsl"),
      ('⧦', "gleichstark"),
      ('⧧', "thermod"),
      ('⧨', "downtriangleleftblack"),
      ('⧩', "downtrianglerightblack"),
      ('⧪', "blackdiamonddownarrow"),
      ('⧫', "mdlgblklozenge"),
      ('⧬', "circledownarrow"),
      ('⧭', "blackcircledownarrow"),
      ('⧮', "errbarsquare"),
      ('⧯', "errbarblacksquare"),
      ('⧰', "errbardiamond"),
      ('⧱', "errbarblackdiamond"),
      ('⧲', "errbarcircle"),
      ('⧳', "errbarblackcircle"),
      ('⧴', "ruledelayed"),
      ('⧵', "setminus"),
      ('⧶', "dsol"),
      ('⧷', "rsolbar"),
      ('⧸', "xsol"),
      ('⧹', "xbsol"),
      ('⧺', "doubleplus"),
      ('⧻', "tripleplus"),
      ('⧼', "lcurvyangle"),
      ('⧽', "rcurvyangle"),
      ('⧾', "tplus"),
      ('⧿', "tminus"),
      ('⨀', "bigodot"),
      ('⨁', "bigoplus"),
      ('⨂', "bigotimes"),
      ('⨃', "bigcupdot"),
      ('⨄', "biguplus"),
      ('⨅', "bigsqcap"),
      ('⨆', "bigsqcup"),
      ('⨇', "conjquant"),
      ('⨈', "disjquant"),
      ('⨉', "bigtimes"),
      ('⨊', "modtwosum"),
      ('⨋', "sumint"),
      ('⨌', "iiiint"),
      ('⨍', "intbar"),
      ('⨎', "intBar"),
      ('⨏', "fint"),
      ('⨐', "cirfnint"),
      ('⨑', "awint"),
      ('⨒', "rppolint"),
      ('⨓', "scpolint"),
      ('⨔', "npolint"),
      ('⨕', "pointint"),
      ('⨖', "sqint"),
      ('⨗', "intlarhk"),
      ('⨘', "intx"),
      ('⨙', "intcap"),
      ('⨚', "intcup"),
      ('⨛', "upint"),
      ('⨜', "lowint"),
      ('⨝', "Join"),
      ('⨞', "bigtriangleleft"),
      ('⨟', "zcmp"),
      ('⨠', "zpipe"),
      ('⨡', "zproject"),
      ('⨢', "ringplus"),
      ('⨣', "plushat"),
      ('⨤', "simplus"),
      ('⨥', "plusdot"),
      ('⨦', "plussim"),
      ('⨧', "plussubtwo"),
      ('⨨', "plustrif"),
      ('⨩', "commaminus"),
      ('⨪', "minusdot"),
      ('⨫', "minusfdots"),
      ('⨬', "minusrdots"),
      ('⨭', "opluslhrim"),
      ('⨮', "oplusrhrim"),
      ('⨯', "vectimes"),
      ('⨰', "dottimes"),
      ('⨱', "timesbar"),
      ('⨲', "btimes"),
      ('⨳', "smashtimes"),
      ('⨴', "otimeslhrim"),
      ('⨵', "otimesrhrim"),
      ('⨶', "otimeshat"),
      ('⨷', "Otimes"),
      ('⨸', "odiv"),
      ('⨹', "triangleplus"),
      ('⨺', "triangleminus"),
      ('⨻', "triangletimes"),
      ('⨼', "intprod"),
      ('⨽', "intprodr"),
      ('⨾', "fcmp"),
      ('⨿', "amalg"),
      ('⩀', "capdot"),
      ('⩁', "uminus"),
      ('⩂', "barcup"),
      ('⩃', "barcap"),
      ('⩄', "capwedge"),
      ('⩅', "cupvee"),
      ('⩆', "cupovercap"),
      ('⩇', "capovercup"),
      ('⩈', "cupbarcap"),
      ('⩉', "capbarcup"),
      ('⩊', "twocups"),
      ('⩋', "twocaps"),
      ('⩌', "closedvarcup"),
      ('⩍', "closedvarcap"),
      ('⩎', "Sqcap"),
      ('⩏', "Sqcup"),
      ('⩐', "closedvarcupsmashprod"),
      ('⩑', "wedgeodot"),
      ('⩒', "veeodot"),
      ('⩓', "Wedge"),
      ('⩔', "Vee"),
      ('⩕', "wedgeonwedge"),
      ('⩖', "veeonvee"),
      ('⩗', "bigslopedvee"),
      ('⩘', "bigslopedwedge"),
      ('⩙', "veeonwedge"),
      ('⩚', "wedgemidvert"),
      ('⩛', "veemidvert"),
      ('⩜', "midbarwedge"),
      ('⩝', "midbarvee"),
      ('⩞', "doublebarwedge"),
      ('⩟', "wedgebar"),
      ('⩠', "wedgedoublebar"),
      ('⩡', "varveebar"),
      ('⩢', "doublebarvee"),
      ('⩣', "veedoublebar"),
      ('⩤', "dsub"),
      ('⩥', "rsub"),
      ('⩦', "eqdot"),
      ('⩧', "dotequiv"),
      ('⩨', "equivVert"),
      ('⩩', "equivVvert"),
      ('⩪', "dotsim"),
      ('⩫', "simrdots"),
      ('⩬', "simminussim"),
      ('⩭', "congdot"),
      ('⩮', "asteq"),
      ('⩯', "hatapprox"),
      ('⩰', "approxeqq"),
      ('⩱', "eqqplus"),
      ('⩲', "pluseqq"),
      ('⩳', "eqqsim"),
      ('⩴', "Coloneq"),
      ('⩵', "eqeq"),
      ('⩶', "eqeqeq"),
      ('⩷', "ddotseq"),
      ('⩸', "equivDD"),
      ('⩹', "ltcir"),
      ('⩺', "gtcir"),
      ('⩻', "ltquest"),
      ('⩼', "gtquest"),
      ('⩽', "leqslant"),
      ('⩾', "geqslant"),
      ('⩿', "lesdot"),
      ('⪀', "gesdot"),
      ('⪁', "lesdoto"),
      ('⪂', "gesdoto"),
      ('⪃', "lesdotor"),
      ('⪄', "gesdotol"),
      ('⪅', "lessapprox"),
      ('⪆', "gtrapprox"),
      ('⪇', "lneq"),
      ('⪈', "gneq"),
      ('⪉', "lnapprox"),
      ('⪊', "gnapprox"),
      ('⪋', "lesseqqgtr"),
      ('⪌', "gtreqqless"),
      ('⪍', "lsime"),
      ('⪎', "gsime"),
      ('⪏', "lsimg"),
      ('⪐', "gsiml"),
      ('⪑', "lgE"),
      ('⪒', "glE"),
      ('⪓', "lesges"),
      ('⪔', "gesles"),
      ('⪕', "eqslantless"),
      ('⪖', "eqslantgtr"),
      ('⪗', "elsdot"),
      ('⪘', "egsdot"),
      ('⪙', "eqqless"),
      ('⪚', "eqqgtr"),
      ('⪛', "eqqslantless"),
      ('⪜', "eqqslantgtr"),
      ('⪝', "simless"),
      ('⪞', "simgtr"),
      ('⪟', "simlE"),
      ('⪠', "simgE"),
      ('⪡', "Lt"),
      ('⪢', "Gt"),
      ('⪣', "partialmeetcontraction"),
      ('⪤', "glj"),
      ('⪥', "gla"),
      ('⪦', "ltcc"),
      ('⪧', "gtcc"),
      ('⪨', "lescc"),
      ('⪩', "gescc"),
      ('⪪', "smt"),
      ('⪫', "lat"),
      ('⪬', "smte"),
      ('⪭', "late"),
      ('⪮', "bumpeqq"),
      ('⪯', "preceq"),
      ('⪰', "succeq"),
      ('⪱', "precneq"),
      ('⪲', "succneq"),
      ('⪳', "preceqq"),
      ('⪴', "succeqq"),
      ('⪵', "precneqq"),
      ('⪶', "succneqq"),
      ('⪷', "precapprox"),
      ('⪸', "succapprox"),
      ('⪹', "precnapprox"),
      ('⪺', "succnapprox"),
      ('⪻', "Prec"),
      ('⪼', "Succ"),
      ('⪽', "subsetdot"),
      ('⪾', "supsetdot"),
      ('⪿', "subsetplus"),
      ('⫀', "supsetplus"),
      ('⫁', "submult"),
      ('⫂', "supmult"),
      ('⫃', "subedot"),
      ('⫄', "supedot"),
      ('⫅', "subseteqq"),
      ('⫆', "supseteqq"),
      ('⫇', "subsim"),
      ('⫈', "supsim"),
      ('⫉', "subsetapprox"),
      ('⫊', "supsetapprox"),
      ('⫋', "subsetneqq"),
      ('⫌', "supsetneqq"),
      ('⫍', "lsqhook"),
      ('⫎', "rsqhook"),
      ('⫏', "csub"),
      ('⫐', "csup"),
      ('⫑', "csube"),
      ('⫒', "csupe"),
      ('⫓', "subsup"),
      ('⫔', "supsub"),
      ('⫕', "subsub"),
      ('⫖', "supsup"),
      ('⫗', "suphsub"),
      ('⫘', "supdsub"),
      ('⫙', "forkv"),
      ('⫚', "topfork"),
      ('⫛', "mlcp"),
      ('⫝̸', "forks"),
      ('⫝', "forksnot"),
      ('⫞', "shortlefttack"),
      ('⫟', "shortdowntack"),
      ('⫠', "shortuptack"),
      ('⫡', "perps"),
      ('⫢', "vDdash"),
      ('⫣', "dashV"),
      ('⫤', "Dashv"),
      ('⫥', "DashV"),
      ('⫦', "varVdash"),
      ('⫧', "Barv"),
      ('⫨', "vBar"),
      ('⫩', "vBarv"),
      ('⫪', "barV"),
      ('⫫', "Vbar"),
      ('⫬', "Not"),
      ('⫭', "bNot"),
      ('⫮', "revnmid"),
      ('⫯', "cirmid"),
      ('⫰', "midcir"),
      ('⫱', "topcir"),
      ('⫲', "nhpar"),
      ('⫳', "parsim"),
      ('⫴', "interleave"),
      ('⫵', "nhVvert"),
      ('⫶', "threedotcolon"),
      ('⫷', "lllnest"),
      ('⫸', "gggnest"),
      ('⫹', "leqqslant"),
      ('⫺', "geqqslant"),
      ('⫻', "trslash"),
      ('⫼', "biginterleave"),
      ('⫽', "sslash"),
      ('⫾', "talloblong"),
      ('⫿', "bigtalloblong"),
      ('⬒', "squaretopblack"),
      ('⬓', "squarebotblack"),
      ('⬔', "squareurblack"),
      ('⬕', "squarellblack"),
      ('⬖', "diamondleftblack"),
      ('⬗', "diamondrightblack"),
      ('⬘', "diamondtopblack"),
      ('⬙', "diamondbotblack"),
      ('⬚', "dottedsquare"),
      ('⬛', "lgblksquare"),
      ('⬜', "lgwhtsquare"),
      ('⬝', "vysmblksquare"),
      ('⬞', "vysmwhtsquare"),
      ('⬟', "pentagonblack"),
      ('⬠', "pentagon"),
      ('⬡', "varhexagon"),
      ('⬢', "varhexagonblack"),
      ('⬣', "hexagonblack"),
      ('⬤', "lgblkcircle"),
      ('⬥', "mdblkdiamond"),
      ('⬦', "mdwhtdiamond"),
      ('⬧', "mdblklozenge"),
      ('⬨', "mdwhtlozenge"),
      ('⬩', "smblkdiamond"),
      ('⬪', "smblklozenge"),
      ('⬫', "smwhtlozenge"),
      ('⬬', "blkhorzoval"),
      ('⬭', "whthorzoval"),
      ('⬮', "blkvertoval"),
      ('⬯', "whtvertoval"),
      ('⬰', "circleonleftarrow"),
      ('⬱', "leftthreearrows"),
      ('⬲', "leftarrowonoplus"),
      ('⬳', "longleftsquigarrow"),
      ('⬴', "nvtwoheadleftarrow"),
      ('⬵', "nVtwoheadleftarrow"),
      ('⬶', "twoheadmapsfrom"),
      ('⬷', "twoheadleftdbkarrow"),
      ('⬸', "leftdotarrow"),
      ('⬹', "nvleftarrowtail"),
      ('⬺', "nVleftarrowtail"),
      ('⬻', "twoheadleftarrowtail"),
      ('⬼', "nvtwoheadleftarrowtail"),
      ('⬽', "nVtwoheadleftarrowtail"),
      ('⬾', "leftarrowx"),
      ('⬿', "leftcurvedarrow"),
      ('⭀', "equalleftarrow"),
      ('⭁', "bsimilarleftarrow"),
      ('⭂', "leftarrowbackapprox"),
      ('⭃', "rightarrowgtr"),
      ('⭄', "rightarrowsupset"),
      ('⭅', "LLeftarrow"),
      ('⭆', "RRightarrow"),
      ('⭇', "bsimilarrightarrow"),
      ('⭈', "rightarrowbackapprox"),
      ('⭉', "similarleftarrow"),
      ('⭊', "leftarrowapprox"),
      ('⭋', "leftarrowbsimilar"),
      ('⭌', "rightarrowbsimilar"),
      ('⭐', "medwhitestar"),
      ('⭑', "medblackstar"),
      ('⭒', "smwhitestar"),
      ('⭓', "rightpentagonblack"),
      ('⭔', "rightpentagon"),
      ('〒', "postalmark"),
      ('〰', "hzigzag"),
      ('ₔ', "_schwa"),
      ('—', "--"),
      ('✔', "Check"),
      ('≠', "neq"),
      ('☐', "Box"),
      ('ħ', "hbar"),
      ('ð', "eth"),
      ('⋄', "diamond"),
      ('◯', "bigcirc"),
      ('∙', "bullet"),
      ('∘', "circ"),
      ('→', "to"),
      ('←', "gets"),
      ('⟹', "implies"),
      ('⟸', "impliedby"),
      ('⟺', "iff"),
      ('∧', "land"),
      ('∨', "lor"),
      ('∅', "emptyset"),
      ('Α', "Alpha"),
      ('Β', "Beta"),
      ('Γ', "Gamma"),
      ('Δ', "Delta"),
      ('Ε', "Epsilon"),
      ('Ζ', "Zeta"),
      ('Η', "Eta"),
      ('Θ', "Theta"),
      ('Ι', "Iota"),
      ('Κ', "Kappa"),
      ('Λ', "Lambda"),
      ('Μ', "Mu"),
      ('Ν', "Nu"),
      ('Ξ', "Xi"),
      ('Ο', "Omicron"),
      ('Π', "Pi"),
      ('Ρ', "Rho"),
      ('Σ', "Sigma"),
      ('Τ', "Tau"),
      ('Υ', "Upsilon"),
      ('Φ', "Phi"),
      ('Χ', "Chi"),
      ('Ψ', "Psi"),
      ('Ω', "Omega"),
      ('α', "alpha"),
      ('β', "beta"),
      ('γ', "gamma"),
      ('δ', "delta"),
      ('ϵ', "epsilon"),
      ('ζ', "zeta"),
      ('η', "eta"),
      ('θ', "theta"),
      ('ι', "iota"),
      ('κ', "kappa"),
      ('λ', "lambda"),
      ('μ', "mu"),
      ('ν', "nu"),
      ('ξ', "xi"),
      ('ο', "omicron"),
      ('π', "pi"),
      ('ρ', "rho"),
      ('ς', "varsigma"),
      ('σ', "sigma"),
      ('τ', "tau"),
      ('υ', "upsilon"),
      ('φ', "varphi"),
      ('χ', "chi"),
      ('ψ', "psi"),
      ('ω', "omega"),
      ('ϑ', "vartheta"),
      ('ϕ', "phi"),
      ('ϖ', "varpi"),
      ('Ϝ', "Digamma"),
      ('ϝ', "digamma"),
      ('ϰ', "varkappa"),
      ('ϱ', "varrho"),
      ('ϴ', "varTheta"),
      ('ε', "varepsilon"),
      ('϶', "backepsilon")
    ]

combMap :: Map Char String
combMap =
  fromList
    [ ('̐', "candra"),
      ('̒', "oturnedcomma"),
      ('̕', "ocommatopright"),
      ('̚', "droang"),
      ('̰', "wideutilde"),
      ('̱', "underbar"),
      ('̸', "not"),
      ('̀', "grave"),
      ('⃗', "vec"),
      ('́', "acute"),
      ('̂', "hat"),
      ('̃', "tilde"),
      ('̄', "bar"),
      ('̅', "overbar"),
      ('̆', "breve"),
      ('̇', "dot"),
      ('̈', "ddot"),
      ('̉', "ovhook"),
      ('̊', "ocirc")
    ]