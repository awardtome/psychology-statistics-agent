// Resizable analysis / conversation / research panels. No data or credentials in storage.
window.panelLayout=(()=>{
 'use strict';
 const body=document.body,button=document.getElementById('research-toggle'),a=document.getElementById('analysis-splitter'),b=document.getElementById('research-splitter');
 let shown=false,chat=360,research=280,active=null,module='home';
 try{const saved=JSON.parse(localStorage.getItem('psych-panel-widths'));if(Number.isFinite(saved?.chat))chat=Math.max(270,Math.min(1000,saved.chat));if(Number.isFinite(saved?.research))research=Math.max(230,Math.min(800,saved.research));}catch{}
 function render(){
  body.classList.add('app-layout');body.classList.toggle('research-visible',shown&&module!=='home');
  const home=module==='home',sidebar=document.querySelector('.sidebar').getBoundingClientRect().width;
  const available=Math.max(600,body.clientWidth-sidebar-(shown?16:8));
  research=Math.max(230,Math.min(research,available-340-270));
  chat=Math.max(270,Math.min(chat,available-340-(shown?research:0)));
  body.style.setProperty('--chat-width',chat+'px');body.style.setProperty('--research-width',research+'px');
  document.getElementById('research-panel').hidden=home||!shown;
  a.hidden=home;b.hidden=home||!shown;button.hidden=home;
  button.textContent=shown?'隐藏科研构思':'科研构思';button.setAttribute('aria-expanded',String(shown));
  a.setAttribute('aria-valuenow',Math.round(chat));b.setAttribute('aria-valuenow',Math.round(research));
 }
 const save=()=>{try{localStorage.setItem('psych-panel-widths',JSON.stringify({chat,research}));}catch{}};
 button.onclick=()=>{shown=!shown;render();};
 for(const handle of [a,b]){
  const resize=(delta,startChat=chat,startResearch=research)=>{if(handle===a)chat=startChat-delta;else{const bounded=Math.max(270-startChat,Math.min(startResearch-230,delta));research=startResearch-bounded;chat=startChat+bounded;}render();};
  handle.onpointerdown=e=>{if(e.button!==0)return;e.preventDefault();active={handle,x:e.clientX,chat,research};handle.setPointerCapture(e.pointerId);body.classList.add('panel-resizing');};
  handle.onpointermove=e=>{if(active?.handle===handle)resize(e.clientX-active.x,active.chat,active.research);};
  const stop=e=>{if(active?.handle!==handle)return;active=null;body.classList.remove('panel-resizing');if(handle.hasPointerCapture(e.pointerId))handle.releasePointerCapture(e.pointerId);save();};
  handle.onpointerup=stop;handle.onpointercancel=stop;
  handle.onkeydown=e=>{if(['ArrowLeft','ArrowRight'].includes(e.key)){e.preventDefault();resize(e.key==='ArrowRight'?20:-20);save();}};
 }
 window.addEventListener('resize',render);
 return {sync(next){module=next;render();}};
})();
