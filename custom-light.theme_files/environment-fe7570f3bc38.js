(globalThis.webpackChunk=globalThis.webpackChunk||[]).push([["environment"],{30523:e=>{e.exports={polyfill:function(){var e,t=window,o=document;if(!("scrollBehavior"in o.documentElement.style)||!0===t.__forceSmoothScrollPolyfill__){var r=t.HTMLElement||t.Element,l={scroll:t.scroll||t.scrollTo,scrollBy:t.scrollBy,elementScroll:r.prototype.scroll||s,scrollIntoView:r.prototype.scrollIntoView},i=t.performance&&t.performance.now?t.performance.now.bind(t.performance):Date.now,n=(e=t.navigator.userAgent,RegExp("MSIE |Trident/|Edge/").test(e))?1:0;t.scroll=t.scrollTo=function(){if(void 0!==arguments[0]){if(!0===c(arguments[0])){l.scroll.call(t,void 0!==arguments[0].left?arguments[0].left:"object"!=typeof arguments[0]?arguments[0]:t.scrollX||t.pageXOffset,void 0!==arguments[0].top?arguments[0].top:void 0!==arguments[1]?arguments[1]:t.scrollY||t.pageYOffset);return}p.call(t,o.body,void 0!==arguments[0].left?~~arguments[0].left:t.scrollX||t.pageXOffset,void 0!==arguments[0].top?~~arguments[0].top:t.scrollY||t.pageYOffset)}},t.scrollBy=function(){if(void 0!==arguments[0]){if(c(arguments[0])){l.scrollBy.call(t,void 0!==arguments[0].left?arguments[0].left:"object"!=typeof arguments[0]?arguments[0]:0,void 0!==arguments[0].top?arguments[0].top:void 0!==arguments[1]?arguments[1]:0);return}p.call(t,o.body,~~arguments[0].left+(t.scrollX||t.pageXOffset),~~arguments[0].top+(t.scrollY||t.pageYOffset))}},r.prototype.scroll=r.prototype.scrollTo=function(){if(void 0!==arguments[0]){if(!0===c(arguments[0])){if("number"==typeof arguments[0]&&void 0===arguments[1])throw SyntaxError("Value could not be converted");l.elementScroll.call(this,void 0!==arguments[0].left?~~arguments[0].left:"object"!=typeof arguments[0]?~~arguments[0]:this.scrollLeft,void 0!==arguments[0].top?~~arguments[0].top:void 0!==arguments[1]?~~arguments[1]:this.scrollTop);return}var e=arguments[0].left,t=arguments[0].top;p.call(this,this,void 0===e?this.scrollLeft:~~e,void 0===t?this.scrollTop:~~t)}},r.prototype.scrollBy=function(){if(void 0!==arguments[0]){if(!0===c(arguments[0])){l.elementScroll.call(this,void 0!==arguments[0].left?~~arguments[0].left+this.scrollLeft:~~arguments[0]+this.scrollLeft,void 0!==arguments[0].top?~~arguments[0].top+this.scrollTop:~~arguments[1]+this.scrollTop);return}this.scroll({left:~~arguments[0].left+this.scrollLeft,top:~~arguments[0].top+this.scrollTop,behavior:arguments[0].behavior})}},r.prototype.scrollIntoView=function(){if(!0===c(arguments[0])){l.scrollIntoView.call(this,void 0===arguments[0]||arguments[0]);return}var e=function(e){for(var t,r,l;e!==o.body&&!1===(r=a(t=e,"Y")&&f(t,"Y"),l=a(t,"X")&&f(t,"X"),r||l);)e=e.parentNode||e.host;return e}(this),r=e.getBoundingClientRect(),i=this.getBoundingClientRect();e!==o.body?(p.call(this,e,e.scrollLeft+i.left-r.left,e.scrollTop+i.top-r.top),"fixed"!==t.getComputedStyle(e).position&&t.scrollBy({left:r.left,top:r.top,behavior:"smooth"})):t.scrollBy({left:i.left,top:i.top,behavior:"smooth"})}}function s(e,t){this.scrollLeft=e,this.scrollTop=t}function c(e){if(null===e||"object"!=typeof e||void 0===e.behavior||"auto"===e.behavior||"instant"===e.behavior)return!0;if("object"==typeof e&&"smooth"===e.behavior)return!1;throw TypeError("behavior member of ScrollOptions "+e.behavior+" is not a valid value for enumeration ScrollBehavior.")}function a(e,t){return"Y"===t?e.clientHeight+n<e.scrollHeight:"X"===t?e.clientWidth+n<e.scrollWidth:void 0}function f(e,o){var r=t.getComputedStyle(e,null)["overflow"+o];return"auto"===r||"scroll"===r}function p(e,r,n){var c,a,f,p,d=i();e===o.body?(c=t,a=t.scrollX||t.pageXOffset,f=t.scrollY||t.pageYOffset,p=l.scroll):(c=e,a=e.scrollLeft,f=e.scrollTop,p=s),function e(o){var r,l,n,s=(i()-o.startTime)/468;r=.5*(1-Math.cos(Math.PI*(s=s>1?1:s))),l=o.startX+(o.x-o.startX)*r,n=o.startY+(o.y-o.startY)*r,o.method.call(o.scrollable,l,n),(l!==o.x||n!==o.y)&&t.requestAnimationFrame(e.bind(t,o))}({scrollable:c,method:p,startTime:d,startX:a,startY:f,x:r,y:n})}}}},15947:(e,t,o)=>{"use strict";var r=o(65809),l=o(71207);l.iG?.addEventListener("error",e=>{e.error&&r.eK(e.error)}),l.iG?.addEventListener("unhandledrejection",async e=>{if(e.promise)try{await e.promise}catch(e){r.eK(e)}}),l.iG?.location.hash==="#b00m"&&setTimeout(()=>{throw Error("b00m")}),o(30523),o(13604);var i=o(47940),n=o(50655);let s="default";i.ZO.createPolicy(s,{createHTML:e=>n.O.apply({policy:()=>e,policyName:s,fallback:e,sanitize:!0,fallbackOnError:!0}),createScript:e=>n.O.apply({policy:()=>e,policyName:s,fallback:e,sanitize:!1,fallbackOnError:!0}),createScriptURL:e=>n.O.apply({policy:()=>e,policyName:s,fallback:e,sanitize:!1,fallbackOnError:!0})});var c=o(18515);(0,o(49237).nn)(),"undefined"!=typeof document&&(()=>{if((0,c.c)("remove_child_patch")&&"function"==typeof Node&&Node.prototype){let e=Node.prototype.removeChild;Node.prototype.removeChild=function(t){try{return e.apply(this,[t])}catch(e){if(e instanceof Error&&e.stack?.includes("react-lib"))return t;throw e}}}})()},79697:(e,t,o)=>{"use strict";let r;o.d(t,{YT:()=>d,qP:()=>u,yM:()=>y});var l=o(88149),i=o(86058),n=o(56363),s=o(43786);let{getItem:c}=(0,n.Z)("localStorage"),a="dimension_",f=["utm_source","utm_medium","utm_campaign","utm_term","utm_content","scid"];try{let e=(0,l.n)("octolytics");delete e.baseContext,r=new i.R(e)}catch(e){}function p(e){let t=(0,l.n)("octolytics").baseContext||{};if(t)for(let[e,o]of(delete t.app_id,delete t.event_url,delete t.host,Object.entries(t)))e.startsWith(a)&&(t[e.replace(a,"")]=o,delete t[e]);let o=document.querySelector("meta[name=visitor-payload]");if(o){let e=JSON.parse(atob(o.content));Object.assign(t,e)}let r=new URLSearchParams(window.location.search);for(let[e,o]of r)f.includes(e.toLowerCase())&&(t[e]=o);return t.staff=(0,s.B)().toString(),Object.assign(t,e)}function d(e){r?.sendPageView(p(e))}function u(e,t={}){let o=document.head?.querySelector('meta[name="current-catalog-service"]')?.content,l=o?{service:o}:{};for(let[e,o]of Object.entries(t))null!=o&&(l[e]=`${o}`);if(r){let t=e||"unknown";p(l),r.sendEvent(t,p(l))}}function y(e){return Object.fromEntries(Object.entries(e).map(([e,t])=>[e,JSON.stringify(t)]))}},50655:(e,t,o)=>{"use strict";o.d(t,{O:()=>a,d:()=>TrustedTypesPolicyError});var r=o(18515),l=o(43786),i=o(65809),n=o(27856),s=o.n(n),c=o(79697);let TrustedTypesPolicyError=class TrustedTypesPolicyError extends Error{};let a={apply:function({policy:e,policyName:t,fallback:o,fallbackOnError:n=!1,sanitize:a,silenceErrorReporting:f=!1}){try{if((0,r.c)("BYPASS_TRUSTED_TYPES_POLICY_RULES"))return o;(0,l.b)({incrementKey:"TRUSTED_TYPES_POLICY_CALLED",trustedTypesPolicyName:t},!1,.1);let i=e();return a&&new Promise(e=>{let o=window.performance.now(),r=s().sanitize(i,{FORBID_ATTR:[]}),l=window.performance.now();if(i.length!==r.length){let n=Error("Trusted Types policy output sanitized"),s=n.stack?.slice(0,1e3),a=i.slice(0,250);(0,c.qP)("trusted_types_policy.sanitize",{policyName:t,output:a,stack:s,outputLength:i.length,sanitizedLength:r.length,executionTime:l-o}),e(i)}}),i}catch(e){if(e instanceof TrustedTypesPolicyError||(f||(0,i.eK)(e),(0,l.b)({incrementKey:"TRUSTED_TYPES_POLICY_ERROR",trustedTypesPolicyName:t}),!n))throw e}return o}}},47940:(e,t,o)=>{"use strict";o.d(t,{ZO:()=>f});var r,l=o(71207),i=o(43786);function n(e){return()=>{throw TypeError(`The policy does not implement the function ${e}`)}}let s={createHTML:n("createHTML"),createScript:n("createScript"),createScriptURL:n("createScriptURL")},c=(r=globalThis).__TRUSTED_TYPE_POLICIES__??(r.__TRUSTED_TYPE_POLICIES__=new Map),a=globalThis.trustedTypes??{createPolicy:(e,t)=>({name:e,...s,...t})},f={createPolicy:(e,t)=>{if(c.has(e))return(0,i.b)({incrementKey:"TRUSTED_TYPES_POLICY_INITIALIZED_TWICE"}),c.get(e);{let o=Object.freeze(a.createPolicy(e,t));return c.set(e,o),o}}},p=!1;l.n4?.addEventListener("securitypolicyviolation",e=>{"require-trusted-types-for"!==e.violatedDirective||p||(console.warn(`Hi fellow Hubber!
    You're probably seeing a Report Only Trusted Types error near this message. This is intended behaviour, staff-only,
    does not impact application control flow, and is used solely for statistic collection. Unfortunately we
    can't gather these statistics without adding the above warnings to your console. Sorry about that!
    Feel free to drop by #pse-architecture if you have any additional questions about Trusted Types or CSP.`),p=!0)})}},e=>{var t=t=>e(e.s=t);e.O(0,["vendors-node_modules_dompurify_dist_purify_js","vendors-node_modules_stacktrace-parser_dist_stack-trace-parser_esm_js-node_modules_github_bro-a4c183","vendors-node_modules_github_hydro-analytics-client_dist_analytics-client_js-node_modules_gith-6a10dd","ui_packages_failbot_failbot_ts"],()=>t(15947)),e.O()}]);
//# sourceMappingURL=environment-34df683b3fc1.js.map