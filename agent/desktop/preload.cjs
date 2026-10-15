const {contextBridge,ipcRenderer}=require('electron');
contextBridge.exposeInMainWorld('meta',{invoke:(method,params)=>ipcRenderer.invoke('meta:invoke',method,params),on:(channel,callback)=>{if(!['state','busy','agentEvent'].includes(channel))throw new Error('Unknown event');ipcRenderer.on(channel,(_event,value)=>callback(value))}});
