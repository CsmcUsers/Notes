# Hermes 指令集速查

> 本文件整理常用 Hermes Agent CLI 指令與對話內 Slash Commands。  
> 最權威、即時的列表請執行：`hermes --help`、`hermes <command> --help`，或在對話中輸入 `/help`、`/commands`。

## 目錄

- [基本啟動](#基本啟動)
- [全域參數](#全域參數)
- [Chat 指令](#chat-指令)
- [設定 Configuration](#設定-configuration)
- [認證與 API Key](#認證與-api-key)
- [工具 Tools](#工具-tools)
- [Skills 技能](#skills-技能)
- [Gateway 通訊平台](#gateway-通訊平台)
- [Sessions 對話紀錄](#sessions-對話紀錄)
- [Cron 排程任務](#cron-排程任務)
- [MCP Servers](#mcp-servers)
- [Profiles](#profiles)
- [Webhooks](#webhooks)
- [其他常用 CLI](#其他常用-cli)
- [對話內 Slash Commands](#對話內-slash-commands)

---

## 基本啟動

```bash
hermes
hermes chat -q "你的問題"
hermes setup
hermes model
hermes doctor
hermes status
```

| 指令 | 用途 |
|---|---|
| `hermes` | 開啟互動式聊天 |
| `hermes chat -q "..."` | 單次提問 |
| `hermes setup` | 初始設定精靈 |
| `hermes model` | 選模型 / provider |
| `hermes doctor` | 檢查環境與設定 |
| `hermes status` | 查看元件狀態 |
| `hermes update` | 更新 Hermes |
| `hermes uninstall` | 移除 Hermes |

---

## 全域參數

```bash
hermes --version
hermes --continue
hermes --resume SESSION
hermes --profile NAME
hermes --skills skill1,skill2
hermes --yolo
```

| 參數 | 用途 |
|---|---|
| `--version`, `-V` | 顯示版本 |
| `--continue`, `-c` | 繼續最近 session |
| `--resume`, `-r` | 指定 session 繼續 |
| `--profile`, `-p` | 使用指定 profile |
| `--skills`, `-s` | 預載技能 |
| `--worktree`, `-w` | 使用隔離 git worktree |
| `--yolo` | 跳過危險命令確認，不建議常開 |
| `--pass-session-id` | 將 session ID 放入 system prompt |

---

## Chat 指令

```bash
hermes chat -q "Hello"
hermes chat -m anthropic/claude-sonnet-4
hermes chat --provider openrouter
hermes chat --toolsets terminal,file,web
hermes chat --checkpoints
```

| 參數 | 用途 |
|---|---|
| `-q`, `--query` | 單次查詢 |
| `-m`, `--model` | 指定模型 |
| `--provider` | 指定 provider |
| `-t`, `--toolsets` | 指定工具集 |
| `-v`, `--verbose` | 詳細輸出 |
| `-Q`, `--quiet` | 安靜模式 |
| `--checkpoints` | 啟用檔案 checkpoint |
| `--source TAG` | 指定 session 來源標籤 |

---

## 設定 Configuration

```bash
hermes setup [section]
hermes model
hermes config
hermes config edit
hermes config set KEY VALUE
hermes config path
hermes config env-path
hermes config check
hermes config migrate
hermes doctor --fix
```

| 指令 | 用途 |
|---|---|
| `hermes setup` | 互動式設定精靈 |
| `hermes setup model` | 設定模型 |
| `hermes setup terminal` | 設定 terminal backend |
| `hermes setup gateway` | 設定 messaging gateway |
| `hermes setup tools` | 設定工具 |
| `hermes setup agent` | 設定 agent |
| `hermes config` | 查看目前設定 |
| `hermes config edit` | 編輯 `config.yaml` |
| `hermes config set KEY VALUE` | 設定某個 config key |
| `hermes config path` | 顯示 config 路徑 |
| `hermes config env-path` | 顯示 `.env` 路徑 |
| `hermes config check` | 檢查設定 |
| `hermes config migrate` | 遷移 / 補新設定 |
| `hermes doctor` | 檢查依賴與設定 |
| `hermes doctor --fix` | 嘗試自動修復 |

常見例子：

```bash
hermes config set model.provider openrouter
hermes config set model.default anthropic/claude-sonnet-4
hermes config set approvals.mode smart
hermes config set memory.memory_enabled true
```

---

## 認證與 API Key

```bash
hermes auth
hermes auth add openai-codex
hermes auth add anthropic
hermes auth list
hermes auth remove PROVIDER INDEX
hermes auth reset PROVIDER
```

| 指令 | 用途 |
|---|---|
| `hermes auth` | 互動式認證管理 |
| `hermes auth add PROVIDER` | 新增 provider 憑證 |
| `hermes auth list [PROVIDER]` | 列出憑證 |
| `hermes auth remove PROVIDER INDEX` | 刪除憑證 |
| `hermes auth reset PROVIDER` | 重設 provider 耗盡狀態 |

---

## 工具 Tools

```bash
hermes tools
hermes tools list
hermes tools enable terminal
hermes tools disable browser
```

| 指令 | 用途 |
|---|---|
| `hermes tools` | 互動式啟用 / 關閉工具 |
| `hermes tools list` | 查看工具狀態 |
| `hermes tools enable NAME` | 啟用工具集 |
| `hermes tools disable NAME` | 關閉工具集 |

常見 toolsets：

```text
web
search
browser
terminal
file
code_execution
vision
image_gen
video
x_search
tts
skills
memory
session_search
delegation
cronjob
clarify
messaging
todo
kanban
debugging
safe
spotify
homeassistant
discord
discord_admin
feishu_doc
feishu_drive
yuanbao
rl
```

> 工具變更通常需要新 session 才會生效：CLI 重新啟動，或對話中 `/reset`。

---

## Skills 技能

```bash
hermes skills list
hermes skills search QUERY
hermes skills browse
hermes skills inspect ID
hermes skills install ID
hermes skills update
hermes skills check
hermes skills uninstall NAME
hermes skills config
hermes skills publish PATH
hermes skills tap add REPO
```

| 指令 | 用途 |
|---|---|
| `skills list` | 列出已安裝技能 |
| `skills search QUERY` | 搜尋技能 |
| `skills browse` | 瀏覽技能目錄 |
| `skills inspect ID` | 預覽技能 |
| `skills install ID` | 安裝技能 |
| `skills update` | 更新技能 |
| `skills check` | 檢查更新 |
| `skills uninstall NAME` | 移除技能 |
| `skills config` | 設定哪些平台可用技能 |
| `skills publish PATH` | 發佈技能 |
| `skills tap add REPO` | 新增 GitHub repo 作為技能來源 |

---

## Gateway 通訊平台

```bash
hermes gateway setup
hermes gateway run
hermes gateway install
hermes gateway start
hermes gateway stop
hermes gateway restart
hermes gateway status
```

| 指令 | 用途 |
|---|---|
| `gateway setup` | 設定通訊平台 |
| `gateway run` | 前景啟動 gateway |
| `gateway install` | 安裝為背景服務 |
| `gateway start` | 啟動服務 |
| `gateway stop` | 停止服務 |
| `gateway restart` | 重啟服務 |
| `gateway status` | 查看 gateway 狀態 |

支援平台包含 Telegram、Discord、Slack、WhatsApp、iMessage、Signal、Email、SMS、Matrix、Microsoft Teams、LINE、Google Chat、Home Assistant、Webhooks 等。

---

## Sessions 對話紀錄

```bash
hermes sessions list
hermes sessions browse
hermes sessions export OUT.jsonl
hermes sessions rename ID "新名稱"
hermes sessions delete ID
hermes sessions prune
hermes sessions stats
```

| 指令 | 用途 |
|---|---|
| `sessions list` | 列出最近 sessions |
| `sessions browse` | 互動式瀏覽 |
| `sessions export OUT` | 匯出 JSONL |
| `sessions rename ID TITLE` | 重新命名 |
| `sessions delete ID` | 刪除 |
| `sessions prune` | 清理舊 sessions |
| `sessions stats` | 統計資訊 |

---

## Cron 排程任務

```bash
hermes cron list
hermes cron create "30m"
hermes cron create "every 2h"
hermes cron create "0 9 * * *"
hermes cron edit ID
hermes cron pause ID
hermes cron resume ID
hermes cron run ID
hermes cron remove ID
hermes cron status
```

| 指令 | 用途 |
|---|---|
| `cron list` | 列出排程 |
| `cron create SCHED` | 建立排程 |
| `cron edit ID` | 編輯排程 |
| `cron pause ID` | 暫停 |
| `cron resume ID` | 恢復 |
| `cron run ID` | 立即觸發 |
| `cron remove ID` | 刪除 |
| `cron status` | 查看排程器狀態 |

排程格式例子：

```text
30m
every 2h
0 9 * * *
2026-06-01T09:00:00
```

---

## MCP Servers

```bash
hermes mcp serve
hermes mcp add NAME --command "..."
hermes mcp add NAME --url "..."
hermes mcp list
hermes mcp test NAME
hermes mcp configure NAME
hermes mcp remove NAME
```

| 指令 | 用途 |
|---|---|
| `mcp serve` | 讓 Hermes 作為 MCP server |
| `mcp add NAME` | 新增 MCP server |
| `mcp list` | 列出 MCP servers |
| `mcp test NAME` | 測試連線 |
| `mcp configure NAME` | 設定工具選擇 |
| `mcp remove NAME` | 移除 MCP server |

---

## Profiles

```bash
hermes profile list
hermes profile create NAME
hermes profile use NAME
hermes profile show NAME
hermes profile rename OLD NEW
hermes profile delete NAME
hermes profile export NAME
hermes profile import FILE
hermes profile alias NAME
```

| 指令 | 用途 |
|---|---|
| `profile list` | 列出 profiles |
| `profile create NAME` | 建立 profile |
| `profile use NAME` | 設為預設 |
| `profile show NAME` | 查看 profile |
| `profile rename A B` | 重新命名 |
| `profile delete NAME` | 刪除 |
| `profile export NAME` | 匯出 |
| `profile import FILE` | 匯入 |
| `profile alias NAME` | 管理 wrapper scripts |

---

## Webhooks

```bash
hermes webhook subscribe NAME
hermes webhook list
hermes webhook test NAME
hermes webhook remove NAME
```

| 指令 | 用途 |
|---|---|
| `webhook subscribe NAME` | 建立 `/webhooks/<name>` 路由 |
| `webhook list` | 列出 webhook subscriptions |
| `webhook test NAME` | 送測試 POST |
| `webhook remove NAME` | 移除 webhook |

---

## 其他常用 CLI

```bash
hermes desktop
hermes gui
hermes dashboard
hermes proxy
hermes portal
hermes plugins list
hermes plugins install NAME
hermes plugins remove NAME
hermes memory setup
hermes memory status
hermes memory off
hermes send
hermes completion bash
hermes completion zsh
hermes insights --days 7
hermes acp
hermes kanban <verb>
hermes pairing list
hermes pairing approve
hermes pairing revoke
hermes secrets bitwarden
hermes claw migrate
```

| 指令 | 用途 |
|---|---|
| `hermes desktop` / `hermes gui` | 啟動桌面 App |
| `hermes dashboard` | 啟動 Web dashboard |
| `hermes proxy` | 啟動 OpenAI-compatible local proxy |
| `hermes portal` | Nous Portal 快速登入 / 設定 |
| `hermes plugins ...` | 管理 plugins |
| `hermes memory ...` | 管理 memory provider |
| `hermes send` | 透過 gateway 發送一次性訊息 |
| `hermes completion bash|zsh` | Shell completion |
| `hermes insights` | 使用量分析 |
| `hermes acp` | ACP server，IDE integration |
| `hermes kanban` | 多 agent work queue |
| `hermes pairing` | DM 授權 |

---

# 對話內 Slash Commands

這些是在 Hermes 對話裡輸入的，不是在終端機。

## Session 控制

```text
/new
/reset
/clear
/retry
/undo
/title 名稱
/compress
/stop
/rollback
/snapshot
/background 任務
/queue 任務
/steer 指令
/agents
/tasks
/resume 名稱
/goal
/redraw
```

| Slash command | 用途 |
|---|---|
| `/new`, `/reset` | 開新 session |
| `/clear` | 清畫面並開新 session，CLI 常用 |
| `/retry` | 重送上一則訊息 |
| `/undo` | 移除上一輪對話 |
| `/title [name]` | 命名 session |
| `/compress` | 手動壓縮 context |
| `/stop` | 停止背景程序 |
| `/rollback [N]` | 回復 filesystem checkpoint |
| `/snapshot` | 建立 / 還原狀態 snapshot |
| `/background <prompt>` | 背景執行任務 |
| `/queue <prompt>` | 排入下一輪 |
| `/steer <prompt>` | 下一次 tool call 後插入指令 |
| `/agents`, `/tasks` | 查看活躍 agents / tasks |
| `/resume [name]` | 繼續指定 session |
| `/goal` | 設定持續目標 |

## 設定

```text
/config
/model
/personality
/reasoning
/verbose
/voice on
/voice tts
/voice off
/yolo
/busy
/indicator
/footer on
/footer off
/skin
/statusbar
```

| Slash command | 用途 |
|---|---|
| `/config` | 顯示設定 |
| `/model [name]` | 查看或切換模型 |
| `/personality [name]` | 設定 personality |
| `/reasoning [level]` | 設定 reasoning 等級 |
| `/verbose` | 切換 verbose 模式 |
| `/voice on|tts|off` | 語音模式 |
| `/yolo` | 切換 approval bypass |
| `/busy` | 控制忙碌時 Enter 行為 |
| `/indicator` | 選 TUI busy indicator |
| `/footer on|off` | 切換 gateway metadata footer |
| `/skin [name]` | 變更主題 |
| `/statusbar` | 切換 status bar |

## 工具 / Skills

```text
/tools
/toolsets
/skills
/skill 技能名
/reload-skills
/reload
/reload-mcp
/cron
/plugins
/curator
/kanban
```

| Slash command | 用途 |
|---|---|
| `/tools` | 管理工具 |
| `/toolsets` | 列出工具集 |
| `/skills` | 搜尋 / 安裝 skills |
| `/skill <name>` | 載入 skill |
| `/reload-skills` | 重新掃描 skills |
| `/reload` | 重新載入 `.env` |
| `/reload-mcp` | 重新載入 MCP servers |
| `/cron` | 管理 cron jobs |
| `/plugins` | 列出 plugins |
| `/curator` | 技能維護 |
| `/kanban` | 多 agent 看板 |

## Gateway

```text
/approve
/deny
/restart
/sethome
/update
/topic
/platforms
/gateway
```

| Slash command | 用途 |
|---|---|
| `/approve` | 核准待確認命令 |
| `/deny` | 拒絕待確認命令 |
| `/restart` | 重啟 gateway |
| `/sethome` | 設定目前 chat 為 home channel |
| `/update` | 更新 Hermes |
| `/topic` | Telegram DM topic sessions |
| `/platforms`, `/gateway` | 查看平台連線狀態 |

## 工具類

```text
/branch
/fork
/handoff 平台
/fast
/browser
/history
/save
/copy
/paste
/image
```

| Slash command | 用途 |
|---|---|
| `/branch`, `/fork` | 分支目前 session |
| `/handoff <platform>` | 轉交 session 到其他平台 |
| `/fast` | 切換 fast processing |
| `/browser` | 開啟 CDP browser connection |
| `/history` | 顯示對話歷史 |
| `/save` | 儲存對話到檔案 |
| `/copy [N]` | 複製最近 assistant 回覆 |
| `/paste` | 附加剪貼簿圖片 |
| `/image` | 附加本機圖片 |

## 查詢資訊

```text
/help
/commands
/usage
/insights
/status
/profile
/debug
```

| Slash command | 用途 |
|---|---|
| `/help` | 顯示說明 |
| `/commands [page]` | 瀏覽全部 commands |
| `/usage` | token 使用量 |
| `/insights [days]` | 使用量分析 |
| `/status` | session 狀態 |
| `/profile` | active profile 資訊 |
| `/debug` | 上傳 debug report 並取得連結 |

## 離開

```text
/quit
/exit
/q
```

---

## 常見查詢方式

```bash
hermes --help
hermes chat --help
hermes gateway --help
hermes skills --help
hermes cron --help
hermes config --help
hermes mcp --help
hermes profile --help
```
