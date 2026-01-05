-module(lustre@server_component).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/lustre/server_component.gleam").
-export([element/2, script/0, route/1, method/1, include/2, subject/1, pid/1, register_subject/1, deregister_subject/1, register_callback/1, deregister_callback/1, emit/2, select/1, runtime_message_decoder/0, client_message_to_json/1]).
-export_type([transport_method/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Server components are an advanced feature that allows you to run components\n"
    " or full Lustre applications on the server. Updates are broadcast to a small\n"
    " (10kb!) client runtime that patches the DOM and events are sent back to the\n"
    " server component in real-time.\n"
    "\n"
    " ```text\n"
    " -- SERVER -----------------------------------------------------------------\n"
    "\n"
    "                  Msg                            Element(Msg)\n"
    " +--------+        v        +----------------+        v        +------+\n"
    " |        | <-------------- |                | <-------------- |      |\n"
    " | update |                 | Lustre runtime |                 | view |\n"
    " |        | --------------> |                | --------------> |      |\n"
    " +--------+        ^        +----------------+        ^        +------+\n"
    "         #(model, Effect(msg))  |        ^          Model\n"
    "                                |        |\n"
    "                                |        |\n"
    "                    DOM patches |        | DOM events\n"
    "                                |        |\n"
    "                                v        |\n"
    "                        +-----------------------+\n"
    "                        |                       |\n"
    "                        | Your WebSocket server |\n"
    "                        |                       |\n"
    "                        +-----------------------+\n"
    "                                |        ^\n"
    "                                |        |\n"
    "                    DOM patches |        | DOM events\n"
    "                                |        |\n"
    "                                v        |\n"
    " -- BROWSER ----------------------------------------------------------------\n"
    "                                |        ^\n"
    "                                |        |\n"
    "                    DOM patches |        | DOM events\n"
    "                                |        |\n"
    "                                v        |\n"
    "                            +----------------+\n"
    "                            |                |\n"
    "                            | Client runtime |\n"
    "                            |                |\n"
    "                            +----------------+\n"
    " ```\n"
    "\n"
    " > **Note**: Lustre's server component runtime is separate from your application's\n"
    " > WebSocket server. You're free to bring your own stack, connect multiple\n"
    " > clients to the same Lustre instance, or keep the application alive even when\n"
    " > no clients are connected.\n"
    "\n"
    " Lustre server components run next to the rest of your backend code, your\n"
    " services, your database, etc. Real-time applications like chat services, games,\n"
    " or components that can benefit from direct access to your backend services\n"
    " like an admin dashboard or data table are excellent candidates for server\n"
    " components.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " Server components are a new feature in Lustre and we're still working on the\n"
    " best ways to use them and show them off. Here are a few examples we've\n"
    " developed so far:\n"
    "\n"
    " - [Basic setup](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/01-basic-setup)\n"
    "\n"
    " - [Custom attributes and events](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/02-attributes-and-events)\n"
    "\n"
    " - [Decoding DOM events](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/03-event-include)\n"
    "\n"
    " - [Connecting more than one client](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/04-multiple-clients)\n"
    "\n"
    " - [Adding publish-subscribe](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/05-publish-subscribe)\n"
    "\n"
    " ## Getting help\n"
    "\n"
    " If you're having trouble with Lustre or not sure what the right way to do\n"
    " something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).\n"
    " You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).\n"
    "\n"
).

-type transport_method() :: web_socket | server_sent_events | polling.

-file("src/lustre/server_component.gleam", 143).
?DOC(
    " Render the server component custom element. This element acts as the thin\n"
    " client runtime for a server component running remotely. There are a handful\n"
    " of attributes you should provide to configure the client runtime:\n"
    "\n"
    " - [`route`](#route) is the URL your server component should connect to. This\n"
    "   **must** be provided before the client runtime will do anything. The route\n"
    "   can be a relative URL, in which case it will be resolved against the current\n"
    "   page URL.\n"
    "\n"
    " - [`method`](#method) is the transport method the client runtime should use.\n"
    "   This defaults to `WebSocket` enabling duplex communication between the client\n"
    "   and server runtime. Other options include `ServerSentEvents` and `Polling`\n"
    "   which are unidirectional transports.\n"
    "\n"
    " > **Note**: the server component runtime bundle must be included and sent to\n"
    " > the client for this to work correctly. You can do this by including the\n"
    " > JavaScript bundle found in Lustre's `priv/static` directory or by inlining\n"
    " > the script source directly with the [`script`](#script) element below.\n"
).
-spec element(
    list(lustre@vdom@vattr:attribute(VIG)),
    list(lustre@vdom@vnode:element(VIG))
) -> lustre@vdom@vnode:element(VIG).
element(Attributes, Children) ->
    lustre@element:element(
        <<"lustre-server-component"/utf8>>,
        Attributes,
        Children
    ).

-file("src/lustre/server_component.gleam", 155).
?DOC(
    " Inline the server component client runtime as a `<script>` tag. Where possible\n"
    " you should prefer serving the pre-built client runtime from Lustre's `priv/static`\n"
    " directory, but this inline script can be useful for development or scenarios\n"
    " where you don't control the HTML document.\n"
).
-spec script() -> lustre@vdom@vnode:element(any()).
script() ->
    lustre@element@html:script(
        [lustre@attribute:type_(<<"module"/utf8>>)],
        <<"var wt=5,ee=Math.pow(2,wt),Tn=ee-1,In=ee/2,Nn=ee/4;var ze=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),jr=new RegExp(`^[${ze}]*`),Cr=new RegExp(`[${ze}]*$`);var v=()=>globalThis?.document,se=\"http://www.w3.org/1999/xhtml\";var Ne=!!globalThis.HTMLElement?.prototype?.moveBefore;var Fe=0;var He=1;var Ve=2;var Ge=0;var le=2;var w=0;var J=1;var ce=2;var We=3;var ae=\"	\";var Ke=0;var Ze=1;var et=2;var tt=3;var nt=4;var rt=5;var it=6;var cn=globalThis.setTimeout,pe=globalThis.clearTimeout,an=(u,e)=>v().createElementNS(u,e),fn=u=>v().createTextNode(u),pn=()=>v().createDocumentFragment(),z=(u,e,t)=>u.insertBefore(e,t),ot=Ne?(u,e,t)=>u.moveBefore(e,t):z,dn=(u,e)=>u.removeChild(e),_n=(u,e)=>u.getAttribute(e),ut=(u,e,t)=>u.setAttribute(e,t),hn=(u,e)=>u.removeAttribute(e),mn=(u,e,t,r)=>u.addEventListener(e,t,r),lt=(u,e,t)=>u.removeEventListener(e,t),xn=(u,e)=>u.innerHTML=e,$n=(u,e)=>u.data=e,y=Symbol(\"lustre\"),_e=class{constructor(e,t,r,n){this.kind=e,this.key=n,this.parent=t,this.children=[],this.node=r,this.handlers=new Map,this.throttles=new Map,this.debouncers=new Map}get parentNode(){return this.kind===w?this.node.parentNode:this.node}};var Q=(u,e,t,r,n)=>{let s=new _e(u,e,t,n);return t[y]=s,e?.children.splice(r,0,s),s},bn=u=>{let e=\"\";for(let t=u[y];t.parent;t=t.parent)if(t.key)e=`${ae}${t.key}${e}`;else{let r=t.parent.children.indexOf(t);e=`${ae}${r}${e}`}return e.slice(1)},P=class{#r=null;#e;#t;#n=!1;constructor(e,t,r,{exposeKeys:n=!1}={}){this.#r=e,this.#e=t,this.#t=r,this.#n=n}mount(e){Q(J,null,this.#r,0,null),this.#$(this.#r,null,this.#r[y],0,e)}push(e){this.#i.push({node:this.#r[y],patch:e}),this.#s()}#i=[];#s(){let e=this.#i;for(;e.length;){let{node:t,patch:r}=e.pop(),{children:n}=t,{changes:s,removed:i,children:o}=r;C(s,l=>this.#o(t,l)),i&&this.#a(t,n.length-i,i),C(o,l=>{let f=n[l.index|0];this.#i.push({node:f,patch:l})})}}#o(e,t){switch(t.kind){case Ke:this.#v(e,t);break;case Ze:this.#b(e,t);break;case et:this.#m(e,t);break;case tt:this.#l(e,t);break;case nt:this.#h(e,t);break;case rt:this.#f(e,t);break;case it:this.#p(e,t);break}}#p(e,{children:t,before:r}){let n=pn(),s=this.#u(e,r);this.#x(n,null,e,r|0,t),z(e.parentNode,n,s)}#f(e,{index:t,with:r}){this.#a(e,t|0,1);let n=this.#u(e,t);this.#$(e.parentNode,n,e,t|0,r)}#u(e,t){t=t|0;let{children:r}=e,n=r.length;if(t<n)return r[t].node;let s=r[n-1];if(!s&&e.kind!==w)return null;for(s||(s=e);s.kind===w&&s.children.length;)s=s.children[s.children.length-1];return s.node.nextSibling}#l(e,{key:t,before:r}){r=r|0;let{children:n,parentNode:s}=e,i=n[r].node,o=n[r];for(let g=r+1;g<n.length;++g){let a=n[g];if(n[g]=o,o=a,a.key===t){n[r]=a;break}}let{kind:l,node:f,children:$}=o;ot(s,f,i),l===w&&this.#c(s,$,i)}#c(e,t,r){for(let n=0;n<t.length;++n){let{kind:s,node:i,children:o}=t[n];ot(e,i,r),s===w&&this.#c(e,o,r)}}#h(e,{index:t}){this.#a(e,t,1)}#a(e,t,r){let{children:n,parentNode:s}=e,i=n.splice(t,r);for(let o=0;o<i.length;++o){let{kind:l,node:f,children:$}=i[o];dn(s,f),this.#d(i[o]),l===w&&i.push(...$)}}#d(e){let{debouncers:t,children:r}=e;for(let{timeout:n}of t.values())n&&pe(n);t.clear(),C(r,n=>this.#d(n))}#m({node:e,handlers:t,throttles:r,debouncers:n},{added:s,removed:i}){C(i,({name:o})=>{t.delete(o)?(lt(e,o,de),this.#_(r,o,0),this.#_(n,o,0)):(hn(e,o),at[o]?.removed?.(e,o))}),C(s,o=>this.#y(e,o))}#v({node:e},{content:t}){$n(e,t??\"\")}#b({node:e},{inner_html:t}){xn(e,t??\"\")}#x(e,t,r,n,s){C(s,i=>this.#$(e,t,r,n++,i))}#$(e,t,r,n,s){switch(s.kind){case J:{let i=this.#g(r,n,s);this.#x(i,null,i[y],0,s.children),z(e,i,t);break}case ce:{let i=this.#w(r,n,s);z(e,i,t);break}case w:{let i=this.#w(r,n,s);z(e,i,t),this.#x(e,t,i[y],0,s.children);break}case We:{let i=this.#g(r,n,s);this.#b({node:i},s),z(e,i,t);break}}}#g(e,t,{kind:r,key:n,tag:s,namespace:i,attributes:o}){let l=an(i||se,s);return Q(r,e,l,t,n),this.#n&&n&&ut(l,\"data-lustre-key\",n),C(o,f=>this.#y(l,f)),l}#w(e,t,{kind:r,key:n,content:s}){let i=fn(s??\"\");return Q(r,e,i,t,n),i}#y(e,t){let{debouncers:r,handlers:n,throttles:s}=e[y],{kind:i,name:o,value:l,prevent_default:f,debounce:$,throttle:g}=t;switch(i){case Fe:{let a=l??\"\";if(o===\"virtual:defaultValue\"){e.defaultValue=a;return}else if(o===\"virtual:defaultChecked\"){e.defaultChecked=!0;return}else if(o===\"virtual:defaultSelected\"){e.defaultSelected=!0;return}a!==_n(e,o)&&ut(e,o,a),at[o]?.added?.(e,a);break}case He:e[o]=l;break;case Ve:{n.has(o)&&lt(e,o,de);let a=f.kind===Ge;mn(e,o,de,{passive:a}),this.#_(s,o,g),this.#_(r,o,$),n.set(o,k=>this.#k(t,k));break}}}#_(e,t,r){let n=e.get(t);if(r>0)n?n.delay=r:e.set(t,{delay:r});else if(n){let{timeout:s}=n;s&&pe(s),e.delete(t)}}#k(e,t){let{currentTarget:r,type:n}=t,{debouncers:s,throttles:i}=r[y],o=bn(r),{prevent_default:l,stop_propagation:f,include:$}=e;l.kind===le&&t.preventDefault(),f.kind===le&&t.stopPropagation(),n===\"submit\"&&(t.detail??={},t.detail.formData=[...new FormData(t.target,t.submitter).entries()]);let g=this.#e(t,o,n,$),a=i.get(n);if(a){let be=Date.now(),gt=a.last||0;be>gt+a.delay&&(a.last=be,a.lastEvent=t,this.#t(t,g))}let k=s.get(n);k&&(pe(k.timeout),k.timeout=cn(()=>{t!==i.get(n)?.lastEvent&&this.#t(t,g)},k.delay)),!a&&!k&&this.#t(t,g)}},C=(u,e)=>{if(Array.isArray(u))for(let t=0;t<u.length;t++)e(u[t]);else if(u)for(u;u.head;u=u.tail)e(u.head)},de=u=>{let{currentTarget:e,type:t}=u;e[y].handlers.get(t)(u)},ct=u=>({added(e){e[u]=!0},removed(e){e[u]=!1}}),gn=u=>({added(e,t){e[u]=t}}),at={checked:ct(\"checked\"),selected:ct(\"selected\"),value:gn(\"value\"),autofocus:{added(u){queueMicrotask(()=>{u.focus?.()})}},autoplay:{added(u){try{u.play?.()}catch(e){console.error(e)}}}};var ft=new WeakMap;async function pt(u){let e=[];for(let r of v().querySelectorAll(\"link[rel=stylesheet], style\"))r.sheet||e.push(new Promise((n,s)=>{r.addEventListener(\"load\",n),r.addEventListener(\"error\",s)}));if(await Promise.allSettled(e),!u.host.isConnected)return[];u.adoptedStyleSheets=u.host.getRootNode().adoptedStyleSheets;let t=[];for(let r of v().styleSheets)try{u.adoptedStyleSheets.push(r)}catch{try{let n=ft.get(r);if(!n){n=new CSSStyleSheet;for(let s of r.cssRules)n.insertRule(s.cssText,n.cssRules.length);ft.set(r,n)}u.adoptedStyleSheets.push(n)}catch{let n=r.ownerNode.cloneNode();u.prepend(n),t.push(n)}}return t}var X=class extends Event{constructor(e,t,r){super(\"context-request\",{bubbles:!0,composed:!0}),this.context=e,this.callback=t,this.subscribe=r}};var dt=0;var _t=1;var ht=2;var mt=3;var K=0;var xt=1;var $t=2;var Z=3;var bt=4;var he=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#r;#e=\"ws\";#t=null;#n=null;#i=[];#s;#o=new Set;#p=new Set;#f=!1;#u=[];#l=new Map;#c=new Set;#h=new MutationObserver(e=>{let t=[];for(let r of e){if(r.type!==\"attributes\")continue;let n=r.attributeName;(!this.#f||this.#o.has(n))&&t.push([n,this.getAttribute(n)])}if(t.length===1){let[r,n]=t[0];this.#n?.send({kind:K,name:r,value:n})}else t.length?this.#n?.send({kind:Z,messages:t.map(([r,n])=>({kind:K,name:r,value:n}))}):this.#u.push(...t)});constructor(){super(),this.internals=this.attachInternals(),this.#h.observe(this,{attributes:!0})}connectedCallback(){for(let e of this.attributes)this.#u.push([e.name,e.value])}attributeChangedCallback(e,t,r){switch(e){case(t!==r&&\"route\"):{this.#t=new URL(r,location.href),this.#a();return}case\"method\":{let n=r.toLowerCase();if(n==this.#e)return;[\"ws\",\"sse\",\"polling\"].includes(n)&&(this.#e=n,this.#e==\"ws\"&&(this.#t.protocol==\"https:\"&&(this.#t.protocol=\"wss:\"),this.#t.protocol==\"http:\"&&(this.#t.protocol=\"ws:\")),this.#a());return}}}async messageReceivedCallback(e){switch(e.kind){case dt:{for(this.#r??=this.attachShadow({mode:e.open_shadow_root?\"open\":\"closed\"});this.#r.firstChild;)this.#r.firstChild.remove();let t=(i,o,l,f)=>{let $=this.#m(i,f??[]);return{kind:xt,path:o,name:l,event:$}},r=(i,o)=>{this.#n?.send(o)};this.#s=new P(this.#r,t,r),this.#o=new Set(e.observed_attributes);let s=this.#u.filter(([i])=>this.#o.has(i)).map(([i,o])=>({kind:K,name:i,value:o}));this.#u=[],this.#p=new Set(e.observed_properties);for(let i of this.#p)Object.defineProperty(this,i,{get(){return this[`_${i}`]},set(o){this[`_${i}`]=o,this.#n?.send({kind:$t,name:i,value:o})}});for(let[i,o]of Object.entries(e.provided_contexts))this.provide(i,o);for(let i of[...new Set(e.requested_contexts)])this.dispatchEvent(new X(i,(o,l)=>{this.#n?.send({kind:bt,key:i,value:o}),this.#c.add(l)}));s.length&&this.#n.send({kind:Z,messages:s}),e.will_adopt_styles&&await this.#d(),this.#r.addEventListener(\"context-request\",i=>{if(!i.context||!i.callback||!this.#l.has(i.context))return;i.stopImmediatePropagation();let o=this.#l.get(i.context);if(i.subscribe){let l=new WeakRef(i.callback),f=()=>{o.subscribers=o.subscribers.filter($=>$!==l)};o.subscribers.push([l,f]),i.callback(o.value,f)}else i.callback(o.value)}),this.#s.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case _t:{this.#s.push(e.patch);break}case ht:{this.dispatchEvent(new CustomEvent(e.name,{detail:e.data}));break}case mt:{this.provide(e.key,e.value);break}}}disconnectedCallback(){for(let e of this.#c)e();this.#c.clear()}provide(e,t){if(!this.#l.has(e))this.#l.set(e,{value:t,subscribers:[]});else{let r=this.#l.get(e);r.value=t;for(let n=r.subscribers.length-1;n>=0;n--){let[s,i]=r.subscribers[n],o=s.deref();if(!o){r.subscribers.splice(n,1);continue}o(t,i)}}}#a(){if(!this.#t||!this.#e)return;this.#n&&this.#n.close();let n={onConnect:()=>{this.#f=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#t,method:this.#e}})},onMessage:s=>{this.messageReceivedCallback(s)},onClose:()=>{this.#f=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\",{detail:{route:this.#t,method:this.#e}}))}};switch(this.#e){case\"ws\":this.#n=new me(this.#t,n);break;case\"sse\":this.#n=new xe(this.#t,n);break;case\"polling\":this.#n=new $e(this.#t,n);break}}async#d(){for(;this.#i.length;)this.#i.pop().remove(),this.#r.firstChild.remove();this.#i=await pt(this.#r)}#m(e,t=[]){let r={};(e.type===\"input\"||e.type===\"change\")&&t.push(\"target.value\"),e.type===\"submit\"&&t.push(\"detail.formData\");for(let n of t){let s=n.split(\".\");for(let i=0,o=e,l=r;i<s.length;i++){if(i===s.length-1){l[s[i]]=o[s[i]];break}l=l[s[i]]??={},o=o[s[i]]}}return r}},me=class{#r;#e;#t=!1;#n=[];#i;#s;#o;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#r=e,this.#e=new WebSocket(this.#r),this.#i=t,this.#s=r,this.#o=n,this.#e.onopen=()=>{this.#i()},this.#e.onmessage=({data:s})=>{try{this.#s(JSON.parse(s))}finally{this.#n.length?this.#e.send(JSON.stringify({kind:Z,messages:this.#n})):this.#t=!1,this.#n=[]}},this.#e.onclose=()=>{this.#o()}}send(e){if(this.#t||this.#e.readyState!==WebSocket.OPEN){this.#n.push(e);return}else this.#e.send(JSON.stringify(e)),this.#t=!0}close(){this.#e.close()}},xe=class{#r;#e;#t;#n;#i;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#r=e,this.#e=new EventSource(this.#r),this.#t=t,this.#n=r,this.#i=n,this.#e.onopen=()=>{this.#t()},this.#e.onmessage=({data:s})=>{try{this.#n(JSON.parse(s))}catch{}}}send(e){}close(){this.#e.close(),this.#i()}},$e=class{#r;#e;#t;#n;#i;#s;constructor(e,{onConnect:t,onMessage:r,onClose:n,...s}){this.#r=e,this.#n=t,this.#i=r,this.#s=n,this.#e=s.interval??5e3,this.#o().finally(()=>{this.#n(),this.#t=setInterval(()=>this.#o(),this.#e)})}async send(e){}close(){clearInterval(this.#t),this.#s()}#o(){return fetch(this.#r).then(e=>e.json()).then(this.#i).catch(console.error)}};customElements.define(\"lustre-server-component\",he);export{he as ServerComponent};"/utf8>>
    ).

-file("src/lustre/server_component.gleam", 170).
?DOC(
    " The `route` attribute tells the client runtime what route it should use to\n"
    " set up the WebSocket connection to the server. Whenever this attribute is\n"
    " changed (by a clientside Lustre app, for example), the client runtime will\n"
    " destroy the current connection and set up a new one.\n"
).
-spec route(binary()) -> lustre@vdom@vattr:attribute(any()).
route(Path) ->
    lustre@attribute:attribute(<<"route"/utf8>>, Path).

-file("src/lustre/server_component.gleam", 176).
?DOC("\n").
-spec method(transport_method()) -> lustre@vdom@vattr:attribute(any()).
method(Value) ->
    lustre@attribute:attribute(<<"method"/utf8>>, case Value of
            web_socket ->
                <<"ws"/utf8>>;

            server_sent_events ->
                <<"sse"/utf8>>;

            polling ->
                <<"polling"/utf8>>
        end).

-file("src/lustre/server_component.gleam", 213).
?DOC(
    " Properties of a JavaScript event object are typically not serialisable. This\n"
    " means if we want to send them to the server we need to make a copy of any\n"
    " fields we want to decode first.\n"
    "\n"
    " This attribute tells Lustre what properties to include from an event. Properties\n"
    " can come from nested fields by using dot notation. For example, you could include\n"
    " the\n"
    " `id` of the target `element` by passing `[\"target.id\"]`.\n"
    "\n"
    " ```gleam\n"
    " import gleam/dynamic/decode\n"
    " import lustre/element.{type Element}\n"
    " import lustre/element/html\n"
    " import lustre/event\n"
    " import lustre/server_component\n"
    "\n"
    " pub fn custom_button(on_click: fn(String) -> msg) -> Element(msg) {\n"
    "   let handler = fn(event) {\n"
    "     use id <- decode.at([\"target\", \"id\"], decode.string)\n"
    "     decode.success(on_click(id))\n"
    "   }\n"
    "\n"
    "   html.button(\n"
    "     [server_component.include([\"target.id\"]), event.on(\"click\", handler)],\n"
    "     [html.text(\"Click me!\")],\n"
    "   )\n"
    " }\n"
    " ```\n"
).
-spec include(lustre@vdom@vattr:attribute(VIS), list(binary())) -> lustre@vdom@vattr:attribute(VIS).
include(Event, Properties) ->
    case Event of
        {event, _, _, _, _, _, _, _, _} ->
            {event,
                erlang:element(2, Event),
                erlang:element(3, Event),
                erlang:element(4, Event),
                Properties,
                erlang:element(6, Event),
                erlang:element(7, Event),
                erlang:element(8, Event),
                erlang:element(9, Event)};

        _ ->
            Event
    end.

-file("src/lustre/server_component.gleam", 233).
?DOC(
    " Recover the `Subject` of the server component runtime so that it can be used\n"
    " in supervision trees or passed to other processes. If you want to hand out\n"
    " different `Subject`s to send messages to your application, take a look at the\n"
    " [`select`](#select) effect.\n"
    "\n"
    " > **Note**: this function is not available on the JavaScript target.\n"
).
-spec subject(lustre:runtime(VIW)) -> gleam@erlang@process:subject(lustre@runtime@server@runtime:message(VIW)).
subject(Runtime) ->
    gleam@function:identity(Runtime).

-file("src/lustre/server_component.gleam", 245).
?DOC(
    " Recover the `Pid` of the server component runtime so that it can be used in\n"
    " supervision trees or passed to other processes. If you want to hand out\n"
    " different `Subject`s to send messages to your application, take a look at the\n"
    " [`select`](#select) effect.\n"
    "\n"
    " > **Note**: this function is not available on the JavaScript target.\n"
).
-spec pid(lustre:runtime(any())) -> gleam@erlang@process:pid_().
pid(Runtime) ->
    Pid@1 = case gleam@erlang@process:subject_owner(subject(Runtime)) of
        {ok, Pid} -> Pid;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"lustre/server_component"/utf8>>,
                        function => <<"pid"/utf8>>,
                        line => 248,
                        value => _assert_fail,
                        start => 21855,
                        'end' => 21915,
                        pattern_start => 21866,
                        pattern_end => 21873})
    end,
    Pid@1.

-file("src/lustre/server_component.gleam", 264).
?DOC(
    " Register a `Subject` to receive messages and updates from Lustre's server\n"
    " component runtime. The process that owns this will be monitored and the\n"
    " subject will be gracefully removed if the process dies.\n"
    "\n"
    " > **Note**: if you are developing a server component for the JavaScript runtime,\n"
    " > you should use [`register_callback`](#register_callback) instead.\n"
).
-spec register_subject(
    gleam@erlang@process:subject(lustre@runtime@transport:client_message(VJE))
) -> lustre@runtime@server@runtime:message(VJE).
register_subject(Client) ->
    {client_registered_subject, Client}.

-file("src/lustre/server_component.gleam", 274).
?DOC(
    " Deregister a `Subject` to stop receiving messages and updates from Lustre's\n"
    " server component runtime. The subject should first have been registered with\n"
    " [`register_subject`](#register_subject) otherwise this will do nothing.\n"
).
-spec deregister_subject(
    gleam@erlang@process:subject(lustre@runtime@transport:client_message(VJI))
) -> lustre@runtime@server@runtime:message(VJI).
deregister_subject(Client) ->
    {client_deregistered_subject, Client}.

-file("src/lustre/server_component.gleam", 288).
?DOC(
    " Register a callback to be called whenever the server component runtime\n"
    " produces a message. Avoid using anonymous functions with this function, as\n"
    " they cannot later be removed using [`deregister_callback`](#deregister_callback).\n"
    "\n"
    " > **Note**: server components running on the Erlang target are **strongly**\n"
    " > encouraged to use [`register_subject`](#register_subject) instead of this\n"
    " > function.\n"
).
-spec register_callback(
    fun((lustre@runtime@transport:client_message(VJM)) -> nil)
) -> lustre@runtime@server@runtime:message(VJM).
register_callback(Callback) ->
    {client_registered_callback, Callback}.

-file("src/lustre/server_component.gleam", 302).
?DOC(
    " Deregister a callback to be called whenever the server component runtime\n"
    " produces a message. The callback to remove is determined by function equality\n"
    " and must be the same function that was passed to [`register_callback`](#register_callback).\n"
    "\n"
    " > **Note**: server components running on the Erlang target are **strongly**\n"
    " > encouraged to use [`register_subject`](#register_subject) instead of this\n"
    " > function.\n"
).
-spec deregister_callback(
    fun((lustre@runtime@transport:client_message(VJP)) -> nil)
) -> lustre@runtime@server@runtime:message(VJP).
deregister_callback(Callback) ->
    {client_deregistered_callback, Callback}.

-file("src/lustre/server_component.gleam", 318).
?DOC(
    " Instruct any connected clients to emit a DOM event with the given name and\n"
    " data. This lets your server component communicate to the frontend the same way\n"
    " any other HTML elements do: you might emit a `\"change\"` event when some part\n"
    " of the server component's state changes, for example.\n"
    "\n"
    " This is a real DOM event and any JavaScript on the page can attach an event\n"
    " listener to the server component element and listen for these events.\n"
).
-spec emit(binary(), gleam@json:json()) -> lustre@effect:effect(any()).
emit(Event, Data) ->
    lustre@effect:event(Event, Data).

-file("src/lustre/server_component.gleam", 341).
?DOC(
    " On the Erlang target, Lustre's server component runtime is an OTP\n"
    " [actor](https://hexdocs.pm/gleam_otp/gleam/otp/actor.html) that can be\n"
    " communicated with using the standard process API and the `Subject` returned\n"
    " when starting the server component.\n"
    "\n"
    " Sometimes, you might want to hand a different `Subject` to a process to restrict\n"
    " the type of messages it can send or to distinguish messages from different\n"
    " sources from one another. The `select` effect creates a fresh `Subject` each\n"
    " time it is run. By returning a `Selector` you can teach the Lustre server\n"
    " component runtime how to listen to messages from this `Subject`.\n"
    "\n"
    " The `select` effect also gives you the dispatch function passed to `effect.from`.\n"
    " This is useful in case you want to store the provided `Subject` in your model\n"
    " for later use. For example you may subscribe to a pubsub service and later use\n"
    " that same `Subject` to unsubscribe.\n"
    "\n"
    " > **Note**: This effect does nothing on the JavaScript runtime, where `Subject`s\n"
    " > and `Selector`s don't exist, and is the equivalent of returning `effect.none()`.\n"
).
-spec select(
    fun((fun((VJU) -> nil), gleam@erlang@process:subject(any())) -> gleam@erlang@process:selector(VJU))
) -> lustre@effect:effect(VJU).
select(Sel) ->
    lustre@effect:select(Sel).

-file("src/lustre/server_component.gleam", 354).
?DOC(
    " The server component client runtime sends JSON-encoded messages for the server\n"
    " runtime to execute. Because your own WebSocket server sits between the two\n"
    " parts of the runtime, you need to decode these actions and pass them to the\n"
    " server runtime yourself.\n"
).
-spec runtime_message_decoder() -> gleam@dynamic@decode:decoder(lustre@runtime@server@runtime:message(any())).
runtime_message_decoder() ->
    gleam@dynamic@decode:map(
        lustre@runtime@transport:server_message_decoder(),
        fun(Field@0) -> {client_dispatched_message, Field@0} end
    ).

-file("src/lustre/server_component.gleam", 370).
?DOC(
    " Encode a message you can send to the client runtime to respond to. The server\n"
    " component runtime will send messages to any registered clients to instruct\n"
    " them to update their DOM or emit events, for example.\n"
    "\n"
    " Because your WebSocket server sits between the two parts of the runtime, you\n"
    " need to encode these actions and send them to the client runtime yourself.\n"
).
-spec client_message_to_json(lustre@runtime@transport:client_message(any())) -> gleam@json:json().
client_message_to_json(Message) ->
    lustre@runtime@transport:client_message_to_json(Message).
