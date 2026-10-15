# 开发与打包

使用者直接下载安装包即可。以下步骤用于从本独立仓库开发和构建 Windows 安装版。

## 软件测试

使用 Node 24，在 `agent/desktop` 中运行：

```text
npm ci
npm test
```

无图形计算环境的持续集成可设置 `ELECTRON_SKIP_BINARY_DOWNLOAD=1` 后安装依赖并运行软件测试；这只验证软件与模拟 API 流程，不等同于真实计算引擎或安装测试。

## 准备本地计算环境

开发机器需要 Windows R 4.5.1 和 `agent/desktop/scripts/dependency-manifest.R` 列出的 R 包。先在已授权的 R 开发会话里设置 `agent_repository` 为本仓库根目录，再 source 该脚本，生成本机的 `agent/runtime-dependencies.csv`。该文件包含本机路径，不提交到 Git。

回到 `agent/desktop`，运行：

```text
python scripts/prepare-runtime.py --r-home C:/你的R安装目录/R-4.5.1
```

脚本复制现有 R 安装及所需依赖到忽略提交的 `runtime` 目录，从 `vendor/meta3level` 复制元分析源码，并准备后端工作流与合成示例。它不修改源 R 安装。依赖源码快照、工作流与软件代码都在本独立仓库中。

## 验证与打包

```text
node scripts/smoke-worker.cjs
node scripts/smoke-research.cjs
npm run build
node scripts/smoke-desktop.cjs
python scripts/make-delivery.py
node scripts/smoke-installer.cjs
```

交付目录默认位于仓库旁的 `PsychologyStatisticsAgent-0.8.0-standalone`。安装验证使用该目录下专用的中文路径，并在验证后移除临时安装、恢复原有快捷方式。不要同时运行安装/卸载测试与桌面程序测试。

模型流程测试使用本机模拟服务，勿将真实密钥或受访者数据加入测试夹具。发布前核对安装包和源码归档的 SHA-256，并确认打包源码、统计引擎、报告和实际安装行为。
