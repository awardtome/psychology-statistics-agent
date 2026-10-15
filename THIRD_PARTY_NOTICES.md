# 第三方来源与许可证

本项目的桌面程序源码采用根目录的 MIT 许可证。各独立依赖继续适用其自己的许可证。

- `vendor/meta3level` 是 [awardtome/meta3level](https://github.com/awardtome/meta3level) 在 `06b39e8f9e8973ca53685888892326dbe269ef0d` 的元分析计算源码快照，保留原 MIT 许可证与作者信息。迁移时 R、man、inst 等统计源码与上游版本逐文件核对。
- 安装包包含 R 4.5.1 及方法包；R 的 COPYING、各包的 DESCRIPTION/LICENSE 与计算环境一起保留。`build/THIRD-PARTY-PACKAGES-0.8.0.json` 记录包名、版本和声明许可证。
- Electron/Chromium 组件及其许可证由安装包随附。Node 开发依赖的版本见 `agent/desktop/package-lock.json`。
- 科研构思的角色和工具设计参照 [IdeaScientist](https://github.com/jiarui-liu/IdeaScientist) 独立实现。没有复制该仓库源码或训练权重，也不声称复现其训练或评测成绩。
- 可选的 [Svalbard Idea Vault](https://huggingface.co/datasets/Jerry999/SvalbardIdeaVault) 由使用者另行导入。其生成注释适用原数据集声明的 CC BY 4.0；它们不是原论文全文，使用时应核对并引用原始来源。安装包和源码归档不包含完整语料。
- NIRApost 与 GGMNIRA 外部计算任务按说明从其原仓库安装依赖，来源和固定修订保留在导出脚本中。

