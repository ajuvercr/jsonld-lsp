import{S as vt,i as yt,s as Mt,C as Ce,k as m,l as w,m as M,h as b,D as Ze,E as Ft,b as he,F as u,G as an,H as Xt,I as ot,J as on,q as k,r as L,u as xn,K as Tn,e as Jt,L as kn,M as ln,N as cn,O as un,P as gn,g as lt,d as ct,Q as Ln,j as An,y as fn,z as hn,A as dn,R as In,B as pn,a as D,c as B,n as T,T as wt}from"../chunks/index.f2252846.js";const Cn=!0,Pn=!0,Dn="ignore",Js=Object.freeze(Object.defineProperty({__proto__:null,prerender:Cn,ssr:Pn,trailingSlash:Dn},Symbol.toStringTag,{value:"Module"}));function Bn(e){return e&&e.__esModule&&Object.prototype.hasOwnProperty.call(e,"default")?e.default:e}function _n(e){return e instanceof Map?e.clear=e.delete=e.set=function(){throw new Error("map is read-only")}:e instanceof Set&&(e.add=e.clear=e.delete=function(){throw new Error("set is read-only")}),Object.freeze(e),Object.getOwnPropertyNames(e).forEach(t=>{const n=e[t],i=typeof n;(i==="object"||i==="function")&&!Object.isFrozen(n)&&_n(n)}),e}class Zt{constructor(t){t.data===void 0&&(t.data={}),this.data=t.data,this.isMatchIgnored=!1}ignoreMatch(){this.isMatchIgnored=!0}}function bn(e){return e.replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;").replace(/'/g,"&#x27;")}function de(e,...t){const n=Object.create(null);for(const i in e)n[i]=e[i];return t.forEach(function(i){for(const g in i)n[g]=i[g]}),n}const Hn="</span>",Yt=e=>!!e.scope,jn=(e,{prefix:t})=>{if(e.startsWith("language:"))return e.replace("language:","language-");if(e.includes(".")){const n=e.split(".");return[`${t}${n.shift()}`,...n.map((i,g)=>`${i}${"_".repeat(g+1)}`)].join(" ")}return`${t}${e}`};class Un{constructor(t,n){this.buffer="",this.classPrefix=n.classPrefix,t.walk(this)}addText(t){this.buffer+=bn(t)}openNode(t){if(!Yt(t))return;const n=jn(t.scope,{prefix:this.classPrefix});this.span(n)}closeNode(t){Yt(t)&&(this.buffer+=Hn)}value(){return this.buffer}span(t){this.buffer+=`<span class="${t}">`}}const Qt=(e={})=>{const t={children:[]};return Object.assign(t,e),t};class Nt{constructor(){this.rootNode=Qt(),this.stack=[this.rootNode]}get top(){return this.stack[this.stack.length-1]}get root(){return this.rootNode}add(t){this.top.children.push(t)}openNode(t){const n=Qt({scope:t});this.add(n),this.stack.push(n)}closeNode(){if(this.stack.length>1)return this.stack.pop()}closeAllNodes(){for(;this.closeNode(););}toJSON(){return JSON.stringify(this.rootNode,null,4)}walk(t){return this.constructor._walk(t,this.rootNode)}static _walk(t,n){return typeof n=="string"?t.addText(n):n.children&&(t.openNode(n),n.children.forEach(i=>this._walk(t,i)),t.closeNode(n)),t}static _collapse(t){typeof t!="string"&&t.children&&(t.children.every(n=>typeof n=="string")?t.children=[t.children.join("")]:t.children.forEach(n=>{Nt._collapse(n)}))}}class Gn extends Nt{constructor(t){super(),this.options=t}addText(t){t!==""&&this.add(t)}startScope(t){this.openNode(t)}endScope(){this.closeNode()}__addSublanguage(t,n){const i=t.root;n&&(i.scope=`language:${n}`),this.add(i)}toHTML(){return new Un(this,this.options).value()}finalize(){return this.closeAllNodes(),!0}}function Ye(e){return e?typeof e=="string"?e:e.source:null}function En(e){return ye("(?=",e,")")}function Vn(e){return ye("(?:",e,")*")}function Kn(e){return ye("(?:",e,")?")}function ye(...e){return e.map(n=>Ye(n)).join("")}function Wn(e){const t=e[e.length-1];return typeof t=="object"&&t.constructor===Object?(e.splice(e.length-1,1),t):{}}function Ot(...e){return"("+(Wn(e).capture?"":"?:")+e.map(i=>Ye(i)).join("|")+")"}function mn(e){return new RegExp(e.toString()+"|").exec("").length-1}function zn(e,t){const n=e&&e.exec(t);return n&&n.index===0}const qn=/\[(?:[^\\\]]|\\.)*\]|\(\??|\\([1-9][0-9]*)|\\./;function Rt(e,{joinWith:t}){let n=0;return e.map(i=>{n+=1;const g=n;let o=Ye(i),s="";for(;o.length>0;){const a=qn.exec(o);if(!a){s+=o;break}s+=o.substring(0,a.index),o=o.substring(a.index+a[0].length),a[0][0]==="\\"&&a[1]?s+="\\"+String(Number(a[1])+g):(s+=a[0],a[0]==="("&&n++)}return s}).map(i=>`(${i})`).join(t)}const Fn=/\b\B/,wn="[a-zA-Z]\\w*",xt="[a-zA-Z_]\\w*",Sn="\\b\\d+(\\.\\d+)?",vn="(-?)(\\b0[xX][a-fA-F0-9]+|(\\b\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)",yn="\\b(0b[01]+)",Xn="!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|-|-=|/=|/|:|;|<<|<<=|<=|<|===|==|=|>>>=|>>=|>=|>>>|>>|>|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~",Jn=(e={})=>{const t=/^#![ ]*\//;return e.binary&&(e.begin=ye(t,/.*\b/,e.binary,/\b.*/)),de({scope:"meta",begin:t,end:/$/,relevance:0,"on:begin":(n,i)=>{n.index!==0&&i.ignoreMatch()}},e)},Qe={begin:"\\\\[\\s\\S]",relevance:0},Zn={scope:"string",begin:"'",end:"'",illegal:"\\n",contains:[Qe]},Yn={scope:"string",begin:'"',end:'"',illegal:"\\n",contains:[Qe]},Qn={begin:/\b(a|an|the|are|I'm|isn't|don't|doesn't|won't|but|just|should|pretty|simply|enough|gonna|going|wtf|so|such|will|you|your|they|like|more)\b/},gt=function(e,t,n={}){const i=de({scope:"comment",begin:e,end:t,contains:[]},n);i.contains.push({scope:"doctag",begin:"[ ]*(?=(TODO|FIXME|NOTE|BUG|OPTIMIZE|HACK|XXX):)",end:/(TODO|FIXME|NOTE|BUG|OPTIMIZE|HACK|XXX):/,excludeBegin:!0,relevance:0});const g=Ot("I","a","is","so","us","to","at","if","in","it","on",/[A-Za-z]+['](d|ve|re|ll|t|s|n)/,/[A-Za-z]+[-][a-z]+/,/[A-Za-z][a-z]{2,}/);return i.contains.push({begin:ye(/[ ]+/,"(",g,/[.]?[:]?([.][ ]|[ ])/,"){3}")}),i},$n=gt("//","$"),es=gt("/\\*","\\*/"),ts=gt("#","$"),ns={scope:"number",begin:Sn,relevance:0},ss={scope:"number",begin:vn,relevance:0},is={scope:"number",begin:yn,relevance:0},rs={begin:/(?=\/[^/\n]*\/)/,contains:[{scope:"regexp",begin:/\//,end:/\/[gimuy]*/,illegal:/\n/,contains:[Qe,{begin:/\[/,end:/\]/,relevance:0,contains:[Qe]}]}]},as={scope:"title",begin:wn,relevance:0},os={scope:"title",begin:xt,relevance:0},ls={begin:"\\.\\s*"+xt,relevance:0},cs=function(e){return Object.assign(e,{"on:begin":(t,n)=>{n.data._beginMatch=t[1]},"on:end":(t,n)=>{n.data._beginMatch!==t[1]&&n.ignoreMatch()}})};var at=Object.freeze({__proto__:null,MATCH_NOTHING_RE:Fn,IDENT_RE:wn,UNDERSCORE_IDENT_RE:xt,NUMBER_RE:Sn,C_NUMBER_RE:vn,BINARY_NUMBER_RE:yn,RE_STARTERS_RE:Xn,SHEBANG:Jn,BACKSLASH_ESCAPE:Qe,APOS_STRING_MODE:Zn,QUOTE_STRING_MODE:Yn,PHRASAL_WORDS_MODE:Qn,COMMENT:gt,C_LINE_COMMENT_MODE:$n,C_BLOCK_COMMENT_MODE:es,HASH_COMMENT_MODE:ts,NUMBER_MODE:ns,C_NUMBER_MODE:ss,BINARY_NUMBER_MODE:is,REGEXP_MODE:rs,TITLE_MODE:as,UNDERSCORE_TITLE_MODE:os,METHOD_GUARD:ls,END_SAME_AS_BEGIN:cs});function us(e,t){e.input[e.index-1]==="."&&t.ignoreMatch()}function gs(e,t){e.className!==void 0&&(e.scope=e.className,delete e.className)}function fs(e,t){t&&e.beginKeywords&&(e.begin="\\b("+e.beginKeywords.split(" ").join("|")+")(?!\\.)(?=\\b|\\s)",e.__beforeBegin=us,e.keywords=e.keywords||e.beginKeywords,delete e.beginKeywords,e.relevance===void 0&&(e.relevance=0))}function hs(e,t){Array.isArray(e.illegal)&&(e.illegal=Ot(...e.illegal))}function ds(e,t){if(e.match){if(e.begin||e.end)throw new Error("begin & end are not supported with match");e.begin=e.match,delete e.match}}function ps(e,t){e.relevance===void 0&&(e.relevance=1)}const _s=(e,t)=>{if(!e.beforeMatch)return;if(e.starts)throw new Error("beforeMatch cannot be used with starts");const n=Object.assign({},e);Object.keys(e).forEach(i=>{delete e[i]}),e.keywords=n.keywords,e.begin=ye(n.beforeMatch,En(n.begin)),e.starts={relevance:0,contains:[Object.assign(n,{endsParent:!0})]},e.relevance=0,delete n.beforeMatch},bs=["of","and","for","in","not","or","if","then","parent","list","value"],Es="keyword";function Mn(e,t,n=Es){const i=Object.create(null);return typeof e=="string"?g(n,e.split(" ")):Array.isArray(e)?g(n,e):Object.keys(e).forEach(function(o){Object.assign(i,Mn(e[o],t,o))}),i;function g(o,s){t&&(s=s.map(a=>a.toLowerCase())),s.forEach(function(a){const l=a.split("|");i[l[0]]=[o,ms(l[0],l[1])]})}}function ms(e,t){return t?Number(t):ws(e)?0:1}function ws(e){return bs.includes(e.toLowerCase())}const $t={},ve=e=>{console.error(e)},en=(e,...t)=>{console.log(`WARN: ${e}`,...t)},Ie=(e,t)=>{$t[`${e}/${t}`]||(console.log(`Deprecated as of ${e}. ${t}`),$t[`${e}/${t}`]=!0)},ut=new Error;function Nn(e,t,{key:n}){let i=0;const g=e[n],o={},s={};for(let a=1;a<=t.length;a++)s[a+i]=g[a],o[a+i]=!0,i+=mn(t[a-1]);e[n]=s,e[n]._emit=o,e[n]._multi=!0}function Ss(e){if(Array.isArray(e.begin)){if(e.skip||e.excludeBegin||e.returnBegin)throw ve("skip, excludeBegin, returnBegin not compatible with beginScope: {}"),ut;if(typeof e.beginScope!="object"||e.beginScope===null)throw ve("beginScope must be object"),ut;Nn(e,e.begin,{key:"beginScope"}),e.begin=Rt(e.begin,{joinWith:""})}}function vs(e){if(Array.isArray(e.end)){if(e.skip||e.excludeEnd||e.returnEnd)throw ve("skip, excludeEnd, returnEnd not compatible with endScope: {}"),ut;if(typeof e.endScope!="object"||e.endScope===null)throw ve("endScope must be object"),ut;Nn(e,e.end,{key:"endScope"}),e.end=Rt(e.end,{joinWith:""})}}function ys(e){e.scope&&typeof e.scope=="object"&&e.scope!==null&&(e.beginScope=e.scope,delete e.scope)}function Ms(e){ys(e),typeof e.beginScope=="string"&&(e.beginScope={_wrap:e.beginScope}),typeof e.endScope=="string"&&(e.endScope={_wrap:e.endScope}),Ss(e),vs(e)}function Ns(e){function t(s,a){return new RegExp(Ye(s),"m"+(e.case_insensitive?"i":"")+(e.unicodeRegex?"u":"")+(a?"g":""))}class n{constructor(){this.matchIndexes={},this.regexes=[],this.matchAt=1,this.position=0}addRule(a,l){l.position=this.position++,this.matchIndexes[this.matchAt]=l,this.regexes.push([l,a]),this.matchAt+=mn(a)+1}compile(){this.regexes.length===0&&(this.exec=()=>null);const a=this.regexes.map(l=>l[1]);this.matcherRe=t(Rt(a,{joinWith:"|"}),!0),this.lastIndex=0}exec(a){this.matcherRe.lastIndex=this.lastIndex;const l=this.matcherRe.exec(a);if(!l)return null;const _=l.findIndex((I,S)=>S>0&&I!==void 0),N=this.matchIndexes[_];return l.splice(0,_),Object.assign(l,N)}}class i{constructor(){this.rules=[],this.multiRegexes=[],this.count=0,this.lastIndex=0,this.regexIndex=0}getMatcher(a){if(this.multiRegexes[a])return this.multiRegexes[a];const l=new n;return this.rules.slice(a).forEach(([_,N])=>l.addRule(_,N)),l.compile(),this.multiRegexes[a]=l,l}resumingScanAtSamePosition(){return this.regexIndex!==0}considerAll(){this.regexIndex=0}addRule(a,l){this.rules.push([a,l]),l.type==="begin"&&this.count++}exec(a){const l=this.getMatcher(this.regexIndex);l.lastIndex=this.lastIndex;let _=l.exec(a);if(this.resumingScanAtSamePosition()&&!(_&&_.index===this.lastIndex)){const N=this.getMatcher(0);N.lastIndex=this.lastIndex+1,_=N.exec(a)}return _&&(this.regexIndex+=_.position+1,this.regexIndex===this.count&&this.considerAll()),_}}function g(s){const a=new i;return s.contains.forEach(l=>a.addRule(l.begin,{rule:l,type:"begin"})),s.terminatorEnd&&a.addRule(s.terminatorEnd,{type:"end"}),s.illegal&&a.addRule(s.illegal,{type:"illegal"}),a}function o(s,a){const l=s;if(s.isCompiled)return l;[gs,ds,Ms,_s].forEach(N=>N(s,a)),e.compilerExtensions.forEach(N=>N(s,a)),s.__beforeBegin=null,[fs,hs,ps].forEach(N=>N(s,a)),s.isCompiled=!0;let _=null;return typeof s.keywords=="object"&&s.keywords.$pattern&&(s.keywords=Object.assign({},s.keywords),_=s.keywords.$pattern,delete s.keywords.$pattern),_=_||/\w+/,s.keywords&&(s.keywords=Mn(s.keywords,e.case_insensitive)),l.keywordPatternRe=t(_,!0),a&&(s.begin||(s.begin=/\B|\b/),l.beginRe=t(l.begin),!s.end&&!s.endsWithParent&&(s.end=/\B|\b/),s.end&&(l.endRe=t(l.end)),l.terminatorEnd=Ye(l.end)||"",s.endsWithParent&&a.terminatorEnd&&(l.terminatorEnd+=(s.end?"|":"")+a.terminatorEnd)),s.illegal&&(l.illegalRe=t(s.illegal)),s.contains||(s.contains=[]),s.contains=[].concat(...s.contains.map(function(N){return Os(N==="self"?s:N)})),s.contains.forEach(function(N){o(N,l)}),s.starts&&o(s.starts,a),l.matcher=g(l),l}if(e.compilerExtensions||(e.compilerExtensions=[]),e.contains&&e.contains.includes("self"))throw new Error("ERR: contains `self` is not supported at the top-level of a language.  See documentation.");return e.classNameAliases=de(e.classNameAliases||{}),o(e)}function On(e){return e?e.endsWithParent||On(e.starts):!1}function Os(e){return e.variants&&!e.cachedVariants&&(e.cachedVariants=e.variants.map(function(t){return de(e,{variants:null},t)})),e.cachedVariants?e.cachedVariants:On(e)?de(e,{starts:e.starts?de(e.starts):null}):Object.isFrozen(e)?de(e):e}var Rs="11.8.0";class xs extends Error{constructor(t,n){super(t),this.name="HTMLInjectionError",this.html=n}}const St=bn,tn=de,nn=Symbol("nomatch"),Ts=7,Rn=function(e){const t=Object.create(null),n=Object.create(null),i=[];let g=!0;const o="Could not find the language '{}', did you forget to load/include a language module?",s={disableAutodetect:!0,name:"Plain text",contains:[]};let a={ignoreUnescapedHTML:!1,throwUnescapedHTML:!1,noHighlightRe:/^(no-?highlight)$/i,languageDetectRe:/\blang(?:uage)?-([\w-]+)\b/i,classPrefix:"hljs-",cssSelector:"pre code",languages:null,__emitter:Gn};function l(r){return a.noHighlightRe.test(r)}function _(r){let h=r.className+" ";h+=r.parentNode?r.parentNode.className:"";const E=a.languageDetectRe.exec(h);if(E){const O=q(E[1]);return O||(en(o.replace("{}",E[1])),en("Falling back to no-highlight mode for this block.",r)),O?E[1]:"no-highlight"}return h.split(/\s+/).find(O=>l(O)||q(O))}function N(r,h,E){let O="",x="";typeof h=="object"?(O=r,E=h.ignoreIllegals,x=h.language):(Ie("10.7.0","highlight(lang, code, ...args) has been deprecated."),Ie("10.7.0",`Please use highlight(code, options) instead.
https://github.com/highlightjs/highlight.js/issues/2277`),x=r,O=h),E===void 0&&(E=!0);const H={code:O,language:x};$("before:highlight",H);const F=H.result?H.result:I(H.language,H.code,E);return F.code=H.code,$("after:highlight",F),F}function I(r,h,E,O){const x=Object.create(null);function H(c,f){return c.keywords[f]}function F(){if(!p.keywords){C.addText(y);return}let c=0;p.keywordPatternRe.lastIndex=0;let f=p.keywordPatternRe.exec(y),d="";for(;f;){d+=y.substring(c,f.index);const v=G.case_insensitive?f[0].toLowerCase():f[0],A=H(p,v);if(A){const[W,Xe]=A;if(C.addText(d),d="",x[v]=(x[v]||0)+1,x[v]<=Ts&&(ce+=Xe),W.startsWith("_"))d+=f[0];else{const Je=G.classNameAliases[W]||W;V(f[0],Je)}}else d+=f[0];c=p.keywordPatternRe.lastIndex,f=p.keywordPatternRe.exec(y)}d+=y.substring(c),C.addText(d)}function ae(){if(y==="")return;let c=null;if(typeof p.subLanguage=="string"){if(!t[p.subLanguage]){C.addText(y);return}c=I(p.subLanguage,y,!0,K[p.subLanguage]),K[p.subLanguage]=c._top}else c=ne(y,p.subLanguage.length?p.subLanguage:null);p.relevance>0&&(ce+=c.relevance),C.__addSublanguage(c._emitter,c.language)}function P(){p.subLanguage!=null?ae():F(),y=""}function V(c,f){c!==""&&(C.startScope(f),C.addText(c),C.endScope())}function Oe(c,f){let d=1;const v=f.length-1;for(;d<=v;){if(!c._emit[d]){d++;continue}const A=G.classNameAliases[c[d]]||c[d],W=f[d];A?V(W,A):(y=W,F(),y=""),d++}}function Z(c,f){return c.scope&&typeof c.scope=="string"&&C.openNode(G.classNameAliases[c.scope]||c.scope),c.beginScope&&(c.beginScope._wrap?(V(y,G.classNameAliases[c.beginScope._wrap]||c.beginScope._wrap),y=""):c.beginScope._multi&&(Oe(c.beginScope,f),y="")),p=Object.create(c,{parent:{value:p}}),p}function oe(c,f,d){let v=zn(c.endRe,d);if(v){if(c["on:end"]){const A=new Zt(c);c["on:end"](f,A),A.isMatchIgnored&&(v=!1)}if(v){for(;c.endsParent&&c.parent;)c=c.parent;return c}}if(c.endsWithParent)return oe(c.parent,f,d)}function ze(c){return p.matcher.regexIndex===0?(y+=c[0],1):(me=!0,0)}function qe(c){const f=c[0],d=c.rule,v=new Zt(d),A=[d.__beforeBegin,d["on:begin"]];for(const W of A)if(W&&(W(c,v),v.isMatchIgnored))return ze(f);return d.skip?y+=f:(d.excludeBegin&&(y+=f),P(),!d.returnBegin&&!d.excludeBegin&&(y=f)),Z(d,c),d.returnBegin?0:f.length}function ee(c){const f=c[0],d=h.substring(c.index),v=oe(p,c,d);if(!v)return nn;const A=p;p.endScope&&p.endScope._wrap?(P(),V(f,p.endScope._wrap)):p.endScope&&p.endScope._multi?(P(),Oe(p.endScope,c)):A.skip?y+=f:(A.returnEnd||A.excludeEnd||(y+=f),P(),A.excludeEnd&&(y=f));do p.scope&&C.closeNode(),!p.skip&&!p.subLanguage&&(ce+=p.relevance),p=p.parent;while(p!==v.parent);return v.starts&&Z(v.starts,c),A.returnEnd?0:f.length}function U(){const c=[];for(let f=p;f!==G;f=f.parent)f.scope&&c.unshift(f.scope);c.forEach(f=>C.openNode(f))}let X={};function Fe(c,f){const d=f&&f[0];if(y+=c,d==null)return P(),0;if(X.type==="begin"&&f.type==="end"&&X.index===f.index&&d===""){if(y+=h.slice(f.index,f.index+1),!g){const v=new Error(`0 width match regex (${r})`);throw v.languageName=r,v.badRule=X.rule,v}return 1}if(X=f,f.type==="begin")return qe(f);if(f.type==="illegal"&&!E){const v=new Error('Illegal lexeme "'+d+'" for mode "'+(p.scope||"<unnamed>")+'"');throw v.mode=p,v}else if(f.type==="end"){const v=ee(f);if(v!==nn)return v}if(f.type==="illegal"&&d==="")return 1;if(te>1e5&&te>f.index*3)throw new Error("potential infinite loop, way more iterations than matches");return y+=d,d.length}const G=q(r);if(!G)throw ve(o.replace("{}",r)),new Error('Unknown language: "'+r+'"');const le=Ns(G);let Ee="",p=O||le;const K={},C=new a.__emitter(a);U();let y="",ce=0,Y=0,te=0,me=!1;try{if(G.__emitTokens)G.__emitTokens(h,C);else{for(p.matcher.considerAll();;){te++,me?me=!1:p.matcher.considerAll(),p.matcher.lastIndex=Y;const c=p.matcher.exec(h);if(!c)break;const f=h.substring(Y,c.index),d=Fe(f,c);Y=c.index+d}Fe(h.substring(Y))}return C.finalize(),Ee=C.toHTML(),{language:r,value:Ee,relevance:ce,illegal:!1,_emitter:C,_top:p}}catch(c){if(c.message&&c.message.includes("Illegal"))return{language:r,value:St(h),illegal:!0,relevance:0,_illegalBy:{message:c.message,index:Y,context:h.slice(Y-100,Y+100),mode:c.mode,resultSoFar:Ee},_emitter:C};if(g)return{language:r,value:St(h),illegal:!1,relevance:0,errorRaised:c,_emitter:C,_top:p};throw c}}function S(r){const h={value:St(r),illegal:!1,relevance:0,_top:s,_emitter:new a.__emitter(a)};return h._emitter.addText(r),h}function ne(r,h){h=h||a.languages||Object.keys(t);const E=S(r),O=h.filter(q).filter(Ne).map(P=>I(P,r,!1));O.unshift(E);const x=O.sort((P,V)=>{if(P.relevance!==V.relevance)return V.relevance-P.relevance;if(P.language&&V.language){if(q(P.language).supersetOf===V.language)return 1;if(q(V.language).supersetOf===P.language)return-1}return 0}),[H,F]=x,ae=H;return ae.secondBest=F,ae}function De(r,h,E){const O=h&&n[h]||E;r.classList.add("hljs"),r.classList.add(`language-${O}`)}function pe(r){let h=null;const E=_(r);if(l(E))return;if($("before:highlightElement",{el:r,language:E}),r.children.length>0&&(a.ignoreUnescapedHTML||(console.warn("One of your code blocks includes unescaped HTML. This is a potentially serious security risk."),console.warn("https://github.com/highlightjs/highlight.js/wiki/security"),console.warn("The element with unescaped HTML:"),console.warn(r)),a.throwUnescapedHTML))throw new xs("One of your code blocks includes unescaped HTML.",r.innerHTML);h=r;const O=h.textContent,x=E?N(O,{language:E,ignoreIllegals:!0}):ne(O);r.innerHTML=x.value,De(r,E,x.language),r.result={language:x.language,re:x.relevance,relevance:x.relevance},x.secondBest&&(r.secondBest={language:x.secondBest.language,relevance:x.secondBest.relevance}),$("after:highlightElement",{el:r,result:x,text:O})}function Be(r){a=tn(a,r)}const He=()=>{re(),Ie("10.6.0","initHighlighting() deprecated.  Use highlightAll() now.")};function je(){re(),Ie("10.6.0","initHighlightingOnLoad() deprecated.  Use highlightAll() now.")}let ie=!1;function re(){if(document.readyState==="loading"){ie=!0;return}document.querySelectorAll(a.cssSelector).forEach(pe)}function Ue(){ie&&re()}typeof window<"u"&&window.addEventListener&&window.addEventListener("DOMContentLoaded",Ue,!1);function se(r,h){let E=null;try{E=h(e)}catch(O){if(ve("Language definition for '{}' could not be registered.".replace("{}",r)),g)ve(O);else throw O;E=s}E.name||(E.name=r),t[r]=E,E.rawDefinition=h.bind(null,e),E.aliases&&Me(E.aliases,{languageName:r})}function Ge(r){delete t[r];for(const h of Object.keys(n))n[h]===r&&delete n[h]}function _e(){return Object.keys(t)}function q(r){return r=(r||"").toLowerCase(),t[r]||t[n[r]]}function Me(r,{languageName:h}){typeof r=="string"&&(r=[r]),r.forEach(E=>{n[E.toLowerCase()]=h})}function Ne(r){const h=q(r);return h&&!h.disableAutodetect}function be(r){r["before:highlightBlock"]&&!r["before:highlightElement"]&&(r["before:highlightElement"]=h=>{r["before:highlightBlock"](Object.assign({block:h.el},h))}),r["after:highlightBlock"]&&!r["after:highlightElement"]&&(r["after:highlightElement"]=h=>{r["after:highlightBlock"](Object.assign({block:h.el},h))})}function Ve(r){be(r),i.push(r)}function Ke(r){const h=i.indexOf(r);h!==-1&&i.splice(h,1)}function $(r,h){const E=r;i.forEach(function(O){O[E]&&O[E](h)})}function We(r){return Ie("10.7.0","highlightBlock will be removed entirely in v12.0"),Ie("10.7.0","Please use highlightElement now."),pe(r)}Object.assign(e,{highlight:N,highlightAuto:ne,highlightAll:re,highlightElement:pe,highlightBlock:We,configure:Be,initHighlighting:He,initHighlightingOnLoad:je,registerLanguage:se,unregisterLanguage:Ge,listLanguages:_e,getLanguage:q,registerAliases:Me,autoDetection:Ne,inherit:tn,addPlugin:Ve,removePlugin:Ke}),e.debugMode=function(){g=!1},e.safeMode=function(){g=!0},e.versionString=Rs,e.regex={concat:ye,lookahead:En,either:Ot,optional:Kn,anyNumberOfTimes:Vn};for(const r in at)typeof at[r]=="object"&&_n(at[r]);return Object.assign(e,at),e},Pe=Rn({});Pe.newInstance=()=>Rn({});var ks=Pe;Pe.HighlightJS=Pe;Pe.default=Pe;const sn=Bn(ks);function Ls(e){let t;return{c(){t=k(e[2])},l(n){t=L(n,e[2])},m(n,i){he(n,t,i)},p(n,i){i&4&&xn(t,n[2])},d(n){n&&b(t)}}}function As(e){let t,n;return{c(){t=new Tn(!1),n=Jt(),this.h()},l(i){t=kn(i,!1),n=Jt(),this.h()},h(){t.a=n},m(i,g){t.m(e[1],i,g),he(i,n,g)},p(i,g){g&2&&t.p(i[1])},d(i){i&&b(n),i&&t.d()}}}function Is(e){let t,n;function i(l,_){return l[1]?As:Ls}let g=i(e),o=g(e),s=[{"data-language":e[3]},e[4]],a={};for(let l=0;l<s.length;l+=1)a=Ce(a,s[l]);return{c(){t=m("pre"),n=m("code"),o.c(),this.h()},l(l){t=w(l,"PRE",{"data-language":!0});var _=M(t);n=w(_,"CODE",{});var N=M(n);o.l(N),N.forEach(b),_.forEach(b),this.h()},h(){Ze(n,"hljs",!0),Ft(t,a),Ze(t,"langtag",e[0]),Ze(t,"svelte-11sh29b",!0)},m(l,_){he(l,t,_),u(t,n),o.m(n,null)},p(l,[_]){g===(g=i(l))&&o?o.p(l,_):(o.d(1),o=g(l),o&&(o.c(),o.m(n,null))),Ft(t,a=an(s,[_&8&&{"data-language":l[3]},_&16&&l[4]])),Ze(t,"langtag",l[0]),Ze(t,"svelte-11sh29b",!0)},i:Xt,o:Xt,d(l){l&&b(t),o.d()}}}function Cs(e,t,n){const i=["langtag","highlighted","code","languageName"];let g=ot(t,i),{langtag:o=!1}=t,{highlighted:s}=t,{code:a}=t,{languageName:l="plaintext"}=t;return e.$$set=_=>{t=Ce(Ce({},t),on(_)),n(4,g=ot(t,i)),"langtag"in _&&n(0,o=_.langtag),"highlighted"in _&&n(1,s=_.highlighted),"code"in _&&n(2,a=_.code),"languageName"in _&&n(3,l=_.languageName)},[o,s,a,l,g]}class Ps extends vt{constructor(t){super(),yt(this,t,Cs,Is,Mt,{langtag:0,highlighted:1,code:2,languageName:3})}}const Ds=Ps,Bs=e=>({highlighted:e&8}),rn=e=>({highlighted:e[3]});function Hs(e){let t,n;const i=[e[4],{languageName:e[0].name},{langtag:e[2]},{highlighted:e[3]},{code:e[1]}];let g={};for(let o=0;o<i.length;o+=1)g=Ce(g,i[o]);return t=new Ds({props:g}),{c(){fn(t.$$.fragment)},l(o){hn(t.$$.fragment,o)},m(o,s){dn(t,o,s),n=!0},p(o,s){const a=s&31?an(i,[s&16&&In(o[4]),s&1&&{languageName:o[0].name},s&4&&{langtag:o[2]},s&8&&{highlighted:o[3]},s&2&&{code:o[1]}]):{};t.$set(a)},i(o){n||(lt(t.$$.fragment,o),n=!0)},o(o){ct(t.$$.fragment,o),n=!1},d(o){pn(t,o)}}}function js(e){let t;const n=e[6].default,i=ln(n,e,e[5],rn),g=i||Hs(e);return{c(){g&&g.c()},l(o){g&&g.l(o)},m(o,s){g&&g.m(o,s),t=!0},p(o,[s]){i?i.p&&(!t||s&40)&&cn(i,n,o,o[5],t?gn(n,o[5],s,Bs):un(o[5]),rn):g&&g.p&&(!t||s&31)&&g.p(o,t?s:-1)},i(o){t||(lt(g,o),t=!0)},o(o){ct(g,o),t=!1},d(o){g&&g.d(o)}}}function Us(e,t,n){const i=["language","code","langtag"];let g=ot(t,i),{$$slots:o={},$$scope:s}=t,{language:a}=t,{code:l}=t,{langtag:_=!1}=t;const N=Ln();let I="";return An(()=>{I&&N("highlight",{highlighted:I})}),e.$$set=S=>{t=Ce(Ce({},t),on(S)),n(4,g=ot(t,i)),"language"in S&&n(0,a=S.language),"code"in S&&n(1,l=S.code),"langtag"in S&&n(2,_=S.langtag),"$$scope"in S&&n(5,s=S.$$scope)},e.$$.update=()=>{e.$$.dirty&3&&(sn.registerLanguage(a.name,a.register),n(3,I=sn.highlight(l,{language:a.name}).value))},[a,l,_,I,g,s,o]}class Gs extends vt{constructor(t){super(),yt(this,t,Us,js,Mt,{language:0,code:1,langtag:2})}}const Vs=Gs;function Ks(e){const t="\\[=*\\[",n="\\]=*\\]",i={begin:t,end:n,contains:["self"]},g=[e.COMMENT("--(?!"+t+")","$"),e.COMMENT("--"+t,n,{contains:[i],relevance:10})];return{name:"Lua",keywords:{$pattern:e.UNDERSCORE_IDENT_RE,literal:"true false nil",keyword:"and break do else elseif end for goto if in local not or repeat return then until while",built_in:"_G _ENV _VERSION __index __newindex __mode __call __metatable __tostring __len __gc __add __sub __mul __div __mod __pow __concat __unm __eq __lt __le assert collectgarbage dofile error getfenv getmetatable ipairs load loadfile loadstring module next pairs pcall print rawequal rawget rawset require select setfenv setmetatable tonumber tostring type unpack xpcall arg self coroutine resume yield status wrap create running debug getupvalue debug sethook getmetatable gethook setmetatable setlocal traceback setfenv getinfo setupvalue getlocal getregistry getfenv io lines write close flush open output type read stderr stdin input stdout popen tmpfile math log max acos huge ldexp pi cos tanh pow deg tan cosh sinh random randomseed frexp ceil floor rad abs sqrt modf asin min mod fmod log10 atan2 exp sin atan os exit setlocale date getenv difftime remove time clock tmpname rename execute package preload loadlib loaded loaders cpath config path seeall string sub upper len gfind rep find match char dump gmatch reverse byte format gsub lower table setn insert getn foreachi maxn foreach concat sort remove"},contains:g.concat([{className:"function",beginKeywords:"function",end:"\\)",contains:[e.inherit(e.TITLE_MODE,{begin:"([_a-zA-Z]\\w*\\.)*([_a-zA-Z]\\w*:)?[_a-zA-Z]\\w*"}),{className:"params",begin:"\\(",endsWithParent:!0,contains:g}].concat(g)},e.C_NUMBER_MODE,e.APOS_STRING_MODE,e.QUOTE_STRING_MODE,{className:"string",begin:t,end:n,contains:[i],relevance:5}])}}const Ws={name:"lua",register:Ks},zs=Ws;function qs(e){let t,n,i,g,o,s,a,l,_,N,I,S,ne,De,pe,Be,He,je,ie,re,Ue,se,Ge,_e,q,Me,Ne,be,Ve,Ke,$,We,r,h,E,O,x,H,F,ae,P,V,Oe,Z,oe,ze,qe,ee,U,X,Fe,G,le,Ee,p,K,C,y,ce,Y,te,me,c,f,d,v,A,W,Xe,Je,J,we,Tt,ft,Re,ht,dt,xe,pt,_t,ue,Te,bt,Et,Se,ke;const mt=e[2].default,z=ln(mt,e,e[1],null);return Se=new Vs({props:{language:zs,code:e[0]}}),{c(){t=m("br"),n=D(),i=m("header"),g=m("hgroup"),o=m("h1"),s=k("JSON-LD Language Server Demo"),a=D(),l=m("h2"),_=k(`Simple demo application that shows the power and usefulness of adding LSP
      support to JSON-LD.`),N=D(),I=m("main"),S=m("section"),ne=m("h2"),De=k("Editor"),pe=D(),z&&z.c(),Be=D(),He=m("br"),je=D(),ie=m("p"),re=k(`The editor will provide autocompletion on properties defined in the linked
      person.jsonld context. This will be done very greedily but still a
      productivity boost is present.`),Ue=D(),se=m("p"),Ge=k("Autocompletion on defined identifiers can be issued by typing "),_e=m("strong"),q=k("@"),Me=k("."),Ne=D(),be=m("p"),Ve=k("Syntax errors are also gathered by the language server."),Ke=D(),$=m("p"),We=k(`This editor is powered by the json-ld. The editor itself uses the
      codemirror components. A thin wrapper links the codemirror editor with the
      LSP. The LSP itself is compiled to WebAssembly and loaded on the page.`),r=D(),h=m("p"),E=k(`Note that codemirror does not natively support LSPs, the LSP features you
      see here are coded with a small wrapper layer, but features like semantic
      highlighting are not supported for this reason.`),O=D(),x=m("section"),H=m("h2"),F=k("Screencast"),ae=k(`
    Because of the double blind requirements I'm not able to let you integrate the
    LSP with your local editors, this however does give the best experience. To help
    you better understand what it looks like in a real editor, I created a screencast
    of the LSP in VSCode.

    `),P=m("img"),Oe=D(),Z=m("section"),oe=m("h2"),ze=k("Editor compatibility"),qe=k(`
    The main benifit of a LSP is that it can be installed on many platforms. NeoVim
    only requires some configuration because the Language Server protocol is native
    to the editor. Other editors like VSCode require a little bit more work to, to
    create an extension for each supported Language Server, this is considered to
    be a thin wrapper.

    `),ee=m("div"),U=m("article"),X=m("img"),G=D(),le=m("h2"),Ee=k("VS Code"),p=D(),K=m("p"),C=k(`Installing the language server for VS Code is as simple as installing
          the correct extensions. This extension is called `),y=m("em"),ce=k("redacted"),Y=k(` lsp
          and can be found on `),te=m("em"),me=k("redacted"),c=k("."),f=D(),d=m("p"),v=k(`Creating the extension is done with special care to make it a, so
          called, web extension. This makes it also possible to use this
          extension on online VS code instances like `),A=m("a"),W=k("vscode.dev"),Xe=k("."),Je=D(),J=m("article"),we=m("img"),ft=D(),Re=m("h2"),ht=k("NeoVim"),dt=D(),xe=m("p"),pt=k(`NeoVim has native support for Language Servers. NeoVim starts up a
          binary and communicates with it via stdin and stdout. To enable the
          JSON-LD LSP, please install it with cargo. The NeoVim lsp integration
          has a dependency on the LSP binary.`),_t=D(),ue=m("details"),Te=m("summary"),bt=k("Lua configuration"),Et=D(),fn(Se.$$.fragment),this.h()},l(R){t=w(R,"BR",{}),n=B(R),i=w(R,"HEADER",{class:!0});var Q=M(i);g=w(Q,"HGROUP",{});var $e=M(g);o=w($e,"H1",{});var kt=M(o);s=L(kt,"JSON-LD Language Server Demo"),kt.forEach(b),a=B($e),l=w($e,"H2",{});var Lt=M(l);_=L(Lt,`Simple demo application that shows the power and usefulness of adding LSP
      support to JSON-LD.`),Lt.forEach(b),$e.forEach(b),Q.forEach(b),N=B(R),I=w(R,"MAIN",{class:!0});var Le=M(I);S=w(Le,"SECTION",{id:!0});var j=M(S);ne=w(j,"H2",{});var At=M(ne);De=L(At,"Editor"),At.forEach(b),pe=B(j),z&&z.l(j),Be=B(j),He=w(j,"BR",{}),je=B(j),ie=w(j,"P",{});var It=M(ie);re=L(It,`The editor will provide autocompletion on properties defined in the linked
      person.jsonld context. This will be done very greedily but still a
      productivity boost is present.`),It.forEach(b),Ue=B(j),se=w(j,"P",{});var et=M(se);Ge=L(et,"Autocompletion on defined identifiers can be issued by typing "),_e=w(et,"STRONG",{});var Ct=M(_e);q=L(Ct,"@"),Ct.forEach(b),Me=L(et,"."),et.forEach(b),Ne=B(j),be=w(j,"P",{});var Pt=M(be);Ve=L(Pt,"Syntax errors are also gathered by the language server."),Pt.forEach(b),Ke=B(j),$=w(j,"P",{});var Dt=M($);We=L(Dt,`This editor is powered by the json-ld. The editor itself uses the
      codemirror components. A thin wrapper links the codemirror editor with the
      LSP. The LSP itself is compiled to WebAssembly and loaded on the page.`),Dt.forEach(b),r=B(j),h=w(j,"P",{});var Bt=M(h);E=L(Bt,`Note that codemirror does not natively support LSPs, the LSP features you
      see here are coded with a small wrapper layer, but features like semantic
      highlighting are not supported for this reason.`),Bt.forEach(b),j.forEach(b),O=B(Le),x=w(Le,"SECTION",{id:!0});var tt=M(x);H=w(tt,"H2",{});var Ht=M(H);F=L(Ht,"Screencast"),Ht.forEach(b),ae=L(tt,`
    Because of the double blind requirements I'm not able to let you integrate the
    LSP with your local editors, this however does give the best experience. To help
    you better understand what it looks like in a real editor, I created a screencast
    of the LSP in VSCode.

    `),P=w(tt,"IMG",{src:!0,alt:!0}),tt.forEach(b),Oe=B(Le),Z=w(Le,"SECTION",{id:!0});var nt=M(Z);oe=w(nt,"H2",{});var jt=M(oe);ze=L(jt,"Editor compatibility"),jt.forEach(b),qe=L(nt,`
    The main benifit of a LSP is that it can be installed on many platforms. NeoVim
    only requires some configuration because the Language Server protocol is native
    to the editor. Other editors like VSCode require a little bit more work to, to
    create an extension for each supported Language Server, this is considered to
    be a thin wrapper.

    `),ee=w(nt,"DIV",{class:!0});var st=M(ee);U=w(st,"ARTICLE",{class:!0});var ge=M(U);X=w(ge,"IMG",{class:!0,src:!0,alt:!0}),G=B(ge),le=w(ge,"H2",{class:!0});var Ut=M(le);Ee=L(Ut,"VS Code"),Ut.forEach(b),p=B(ge),K=w(ge,"P",{class:!0});var Ae=M(K);C=L(Ae,`Installing the language server for VS Code is as simple as installing
          the correct extensions. This extension is called `),y=w(Ae,"EM",{class:!0});var Gt=M(y);ce=L(Gt,"redacted"),Gt.forEach(b),Y=L(Ae,` lsp
          and can be found on `),te=w(Ae,"EM",{class:!0});var Vt=M(te);me=L(Vt,"redacted"),Vt.forEach(b),c=L(Ae,"."),Ae.forEach(b),f=B(ge),d=w(ge,"P",{class:!0});var it=M(d);v=L(it,`Creating the extension is done with special care to make it a, so
          called, web extension. This makes it also possible to use this
          extension on online VS code instances like `),A=w(it,"A",{target:!0,href:!0,class:!0});var Kt=M(A);W=L(Kt,"vscode.dev"),Kt.forEach(b),Xe=L(it,"."),it.forEach(b),ge.forEach(b),Je=B(st),J=w(st,"ARTICLE",{class:!0});var fe=M(J);we=w(fe,"IMG",{class:!0,src:!0,alt:!0}),ft=B(fe),Re=w(fe,"H2",{class:!0});var Wt=M(Re);ht=L(Wt,"NeoVim"),Wt.forEach(b),dt=B(fe),xe=w(fe,"P",{class:!0});var zt=M(xe);pt=L(zt,`NeoVim has native support for Language Servers. NeoVim starts up a
          binary and communicates with it via stdin and stdout. To enable the
          JSON-LD LSP, please install it with cargo. The NeoVim lsp integration
          has a dependency on the LSP binary.`),zt.forEach(b),_t=B(fe),ue=w(fe,"DETAILS",{class:!0});var rt=M(ue);Te=w(rt,"SUMMARY",{class:!0});var qt=M(Te);bt=L(qt,"Lua configuration"),qt.forEach(b),Et=B(rt),hn(Se.$$.fragment,rt),rt.forEach(b),fe.forEach(b),st.forEach(b),nt.forEach(b),Le.forEach(b),this.h()},h(){T(i,"class","container"),T(S,"id","editor"),wt(P.src,V="./screen.gif")||T(P,"src",V),T(P,"alt","Screencast"),T(x,"id","screencast"),T(X,"class","logo svelte-1wmpp3r"),wt(X.src,Fe="https://www.shanebart.com/wp-content/uploads/2019/05/5k4h36j3h4j.png")||T(X,"src",Fe),T(X,"alt","Visual Studio Code Logo"),T(le,"class","svelte-1wmpp3r"),T(y,"class","svelte-1wmpp3r"),T(te,"class","svelte-1wmpp3r"),T(K,"class","svelte-1wmpp3r"),T(A,"target","_blank"),T(A,"href","https://vscode.dev"),T(A,"class","svelte-1wmpp3r"),T(d,"class","svelte-1wmpp3r"),T(U,"class","article svelte-1wmpp3r"),T(we,"class","logo svelte-1wmpp3r"),wt(we.src,Tt="https://upload.wikimedia.org/wikipedia/commons/thumb/4/4f/Neovim-logo.svg/2560px-Neovim-logo.svg.png")||T(we,"src",Tt),T(we,"alt","Neovim Logo"),T(Re,"class","svelte-1wmpp3r"),T(xe,"class","svelte-1wmpp3r"),T(Te,"class","svelte-1wmpp3r"),T(ue,"class","svelte-1wmpp3r"),T(J,"class","article svelte-1wmpp3r"),T(ee,"class","flow svelte-1wmpp3r"),T(Z,"id","install"),T(I,"class","container")},m(R,Q){he(R,t,Q),he(R,n,Q),he(R,i,Q),u(i,g),u(g,o),u(o,s),u(g,a),u(g,l),u(l,_),he(R,N,Q),he(R,I,Q),u(I,S),u(S,ne),u(ne,De),u(S,pe),z&&z.m(S,null),u(S,Be),u(S,He),u(S,je),u(S,ie),u(ie,re),u(S,Ue),u(S,se),u(se,Ge),u(se,_e),u(_e,q),u(se,Me),u(S,Ne),u(S,be),u(be,Ve),u(S,Ke),u(S,$),u($,We),u(S,r),u(S,h),u(h,E),u(I,O),u(I,x),u(x,H),u(H,F),u(x,ae),u(x,P),u(I,Oe),u(I,Z),u(Z,oe),u(oe,ze),u(Z,qe),u(Z,ee),u(ee,U),u(U,X),u(U,G),u(U,le),u(le,Ee),u(U,p),u(U,K),u(K,C),u(K,y),u(y,ce),u(K,Y),u(K,te),u(te,me),u(K,c),u(U,f),u(U,d),u(d,v),u(d,A),u(A,W),u(d,Xe),u(ee,Je),u(ee,J),u(J,we),u(J,ft),u(J,Re),u(Re,ht),u(J,dt),u(J,xe),u(xe,pt),u(J,_t),u(J,ue),u(ue,Te),u(Te,bt),u(ue,Et),dn(Se,ue,null),ke=!0},p(R,[Q]){z&&z.p&&(!ke||Q&2)&&cn(z,mt,R,R[1],ke?gn(mt,R[1],Q,null):un(R[1]),null)},i(R){ke||(lt(z,R),lt(Se.$$.fragment,R),ke=!0)},o(R){ct(z,R),ct(Se.$$.fragment,R),ke=!1},d(R){R&&b(t),R&&b(n),R&&b(i),R&&b(N),R&&b(I),z&&z.d(R),pn(Se)}}}function Fs(e,t,n){let{$$slots:i={},$$scope:g}=t;const o=`#  Add a config to lspconfig.configs
local configs = require("lspconfig.configs")

configs.jsonld = {
  default_config = {
    cmd = { 'jsonld-language-server' },
    filetypes = { 'jsonld' },
    root_dir = require("lspconfig.util")
      .find_git_ancestor,
    single_file_support = true,
    init_options = {},
  }
}

# Start the LSP
local lspconfig = require("lspconfig")

lspconfig.jsonld.setup {
  on_attach = M.on_attach,
  capabilities = M.capabilities,
}`;return e.$$set=s=>{"$$scope"in s&&n(1,g=s.$$scope)},[o,g,i]}class Zs extends vt{constructor(t){super(),yt(this,t,Fs,qs,Mt,{})}}export{Zs as component,Js as universal};