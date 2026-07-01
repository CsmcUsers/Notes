# GitLab CI/CD 全端專案建置筆記

> 適用情境：.NET 後端 + React/TypeScript 前端（pnpm）的方案，使用兩台 GitLab Runner 分工建置。

---

## 1. 架構概覽

```
主機 (Docker Host)
├── gitlab                  ← GitLab 服務容器
├── grunner-node            ← 前端 Runner（Node/pnpm 環境）
└── grunner-mend            ← Mend SCA 掃描 Runner
    （dotnet NuGet Runner 使用 the-fifth tag，掛載 NuGet cache）
```

### Pipeline 流程

```
dev:build 階段（並行）
├── dev-build-frontend  [gitlab-runner-node]  → artifact: dist/
└── dev-build-backend   [the-fifth]           ← 等待 frontend artifact，再 dotnet publish

dev:scan 階段
└── dev-mend-scan       [gitlab-runner-mend]
```

---

## 2. 關鍵設計決策

### 2.1 前後端分開 Job 的原因

`.csproj` 中有一段條件式參考：

```xml
<ProjectReference Include="..\slnmeetinghub_ins.client\slnmeetinghub_ins.client.esproj"
                  Condition="'$(CI)' != 'true'">
```

- **本機開發**（`CI` 未設定）：`dotnet build` 會自動觸發 `.esproj` 建置前端
- **CI 環境**（`CI=true`）：`dotnet publish` 跳過 `.esproj`，前端需獨立建置

所以 CI 時必須：
1. 前端 Job 先用 pnpm build 產出 `dist/`
2. 後端 Job 用 `dotnet publish`，再把 `dist/` 複製進 `publish/wwwroot/`

### 2.2 `needs` 關鍵字的作用

```yaml
dev-build-backend:
  needs:
    - job: dev-build-frontend
      artifacts: true
```

- 讓後端 Job **不需等待整個 stage 完成**，只要前端 Job 完成就立即啟動
- 同時下載前端 Job 的 artifact，讓 `slnmeetinghub_ins.client/dist/` 可在後端 Job 中使用
- 兩個 Job 事實上接近**並行執行**（前端跑完立刻接後端）

---

## 3. 完整 `.gitlab-ci.yml` 範例

```yaml
stages:
    - dev:build
    - dev:scan

.nuget_cache: &nuget_cache
    cache:
        key: nuget-$CI_COMMIT_REF_SLUG
        paths:
            - .nuget/

dev-build-frontend:
    stage: dev:build
    only:
        - dev
    tags:
        - gitlab-runner-node
    variables:
        PNPM_STORE_DIR: '/opt/pnpm-store'
    script:
        - cd slnmeetinghub_ins.client
        - pnpm config set store-dir "$PNPM_STORE_DIR"
        - pnpm install --frozen-lockfile
        - pnpm build
    artifacts:
        paths:
            - slnmeetinghub_ins.client/dist/
        expire_in: 1 hour

dev-build-backend:
    stage: dev:build
    only:
        - dev
    image: mcr.microsoft.com/dotnet/sdk:10.0
    tags:
        - the-fifth
    needs:
        - job: dev-build-frontend
          artifacts: true
    <<: *nuget_cache
    variables:
        NUGET_PACKAGES: '$CI_PROJECT_DIR/.nuget'
        CI: 'true'
    script:
        - dotnet restore slnMeetingHub_Ins.Server/slnMeetingHub_Ins.Server.csproj
        - dotnet publish slnMeetingHub_Ins.Server/slnMeetingHub_Ins.Server.csproj -c Release -o publish/ --no-restore
        - cp -r slnmeetinghub_ins.client/dist/. publish/wwwroot/
    artifacts:
        paths:
            - publish/
        expire_in: 1 hour

dev-mend-scan:
    stage: dev:scan
    only:
        - dev
    tags:
        - gitlab-runner-mend
    # ... 略
```

---

## 4. 遇到的問題與解法

### 4.1 Runner 容器一直重啟（Restarting）

**症狀**：`docker ps -a` 看到 Runner 容器狀態為 `Restarting (1) 5 seconds ago`，GitLab 顯示 Runner 離線。

**診斷**：
```bash
docker logs grunner-node --tail 50
```

**錯誤訊息**：
```
FATAL: Service run failed  error=decoding configuration file: toml: line 28
(last key "runners.docker.image"): Key 'runners.docker.image' has already been defined.
```

**原因**：`config.toml` 中 `[runners.docker]` 區段有重複的 `image` key，TOML 格式不允許重複 key。

**解法**：用 `vi` 開啟 `config.toml`，找到 `[runners.docker]` 區塊，刪除重複的 `image = "..."` 行，只保留一個，再重啟容器。

---

### 4.2 Job 卡在 Pending（擱置中）

**症狀**：Job 顯示 ⏸ 橘色暫停圖示，一直等不到 Runner 接手。

**常見原因與排查**：

| 原因 | 確認方式 |
|------|---------|
| Runner 容器已崩潰但 GitLab 還沒更新狀態 | `docker ps -a` 確認容器 STATUS |
| `config.toml` 設定錯誤導致啟動失敗 | `docker logs <runner容器名>` |
| Runner tag 名稱與 CI yml 不符 | 比對 GitLab 管理中心的 Runner tag 與 yml 中的 `tags:` |
| 專案未開放使用此 Instance Runner | 專案 → 設定 → CI/CD → 執行器 |

---

### 4.3 pnpm: not found

**症狀**：
```
/bin/sh: eval: line 175: pnpm: not found
ERROR: Job failed: exit code 127
```

**原因**：`node:26-alpine` 映像不包含 pnpm。Node.js 22 以後 corepack 也不再預裝。

**解法一：在 CI script 安裝（每次執行）**
```yaml
script:
    - corepack enable
    - corepack prepare pnpm@11.8.0 --activate
    - pnpm install ...
```
> Node.js 22 以後 corepack 也已移除，改用：
```bash
npm install -g pnpm@11.8.0
```

**解法二：自訂 Docker image（推薦）**

在 Runner 主機建置一次，之後每個 Job 直接使用：
```bash
cat > /opt/node-pnpm.Dockerfile << 'EOF'
FROM node:26-alpine
RUN npm install -g pnpm@11.8.0
EOF

docker build -f /opt/node-pnpm.Dockerfile -t node-pnpm:26 .
docker run --rm node-pnpm:26 pnpm --version  # 驗證
```

然後在 Runner `config.toml` 的 `[runners.docker]` 指定：
```toml
image = "node-pnpm:26"
pull_policy = ["if-not-present"]
```

---

### 4.4 pnpm store ENOENT 錯誤

**症狀**：
```
ENOENT: no such file or directory, mkdir '/opt/pnpm-store/v11'
```

**原因**：有兩種可能
1. `/opt/pnpm-store` **未掛載進 job 容器**（Docker executor 需要在 `config.toml` 設定 volume）
2. 舊版 pnpm store（`v8`/`v9` 格式）與新版 pnpm@11.x 的 `v11` 格式不相容

**解法**：

在 `config.toml` 加入 volume 掛載：
```toml
[runners.docker]
  volumes = ["/opt/pnpm-store:/opt/pnpm-store", "/cache"]
```

清除舊版 store：
```bash
rm -rf /opt/pnpm-store
mkdir -p /opt/pnpm-store
docker restart grunner-node
```

**替代方案：改用 GitLab CI Cache（不需要 volume）**
```yaml
cache:
    key:
        files:
            - slnmeetinghub_ins.client/pnpm-lock.yaml
    paths:
        - .pnpm-store/
variables:
    PNPM_STORE_DIR: '$CI_PROJECT_DIR/.pnpm-store'
```

---

### 4.5 vite build 失敗：Could not create certificate

**症狀**：
```
Error: Could not create certificate.
failed to load config from .../vite.config.ts
```

**原因**：`vite.config.ts` 在載入時會呼叫 `dotnet dev-certs` 建立本機 HTTPS 憑證（本機開發用），但 CI 容器（`node:26-alpine`）沒有安裝 `dotnet`。

**解法**：修改 `vite.config.ts`，偵測 CI 環境時跳過憑證相關邏輯：

```typescript
const isCI = env.CI === 'true' || env.CI === '1';

if (!isCI) {
    if (!fs.existsSync(baseFolder)) {
        fs.mkdirSync(baseFolder, { recursive: true });
    }
    if (!fs.existsSync(certFilePath) || !fs.existsSync(keyFilePath)) {
        if (0 !== child_process.spawnSync('dotnet', ['dev-certs', ...]).status) {
            throw new Error('Could not create certificate.');
        }
    }
}

// server 的 https 設定也需條件式載入
server: {
    ...
    ...(!isCI && {
        https: {
            key: fs.readFileSync(keyFilePath),
            cert: fs.readFileSync(certFilePath),
        },
    }),
}
```

GitLab CI 會自動設定 `CI=true`，所以不需要在 CI yml 額外設定。

---

## 5. Docker Executor vs Shell Executor

| | Docker Executor | Shell Executor |
|--|--|--|
| 每個 Job 用獨立容器 | ✅ | ❌（直接在主機 shell 跑） |
| 需指定 `image:` | 是（或設定預設值） |  否 |
| host volume 存取 | 需在 config.toml 設定 `volumes` | 直接存取 |
| 環境隔離 | ✅ 高 | ❌ 低 |
| 有 pnpm store 在 `/opt/pnpm-store` | 需掛載 volume | 直接可用 |

---

## 6. 合併 image vs 分開 Runner 的選擇

| 比較項目 | 合併 image（Node + .NET） | 分開 Runner（目前方式） |
|---------|--------------------------|------------------------|
| CI 設定複雜度 | 低（一個 Job） | 較高（需 needs + artifact） |
| 建置速度 | 慢（串行） | 快（並行） |
| Image 大小 | 大（1~2 GB） | 各自小 |
| 適合場景 | 小型專案、快速迭代 | 中大型專案、效能優先 |

**結論**：分開 Runner 是更專業的做法，初期設定麻煩，但長期並行建置時間優勢明顯。

---

## 7. Runner config.toml 重要設定摘要

```toml
[[runners]]
  name = "gitlab-runner-node-forNpm"
  url = "https://your-gitlab-url/"
  token = "xxxxxxxxxxxx"
  executor = "docker"

  # 可選：每個 Job 前執行（用於安裝工具等）
  # pre_build_script = "npm install -g pnpm@11.8.0"

  [runners.docker]
    image = "node-pnpm:26"                        # 預設 image
    pull_policy = ["if-not-present"]               # 優先用本機 image
    volumes = ["/opt/pnpm-store:/opt/pnpm-store",  # pnpm store 掛載
               "/cache"]
```

---

## 8. 常用除錯指令

```bash
# 查看 Runner 容器狀態
docker ps -a

# 查看 Runner 崩潰原因
docker logs grunner-node --tail 50

# 查看 config.toml 位置（volume 掛載）
docker inspect grunner-node | grep -A10 "Mounts"

# 驗證自訂 image 是否正確
docker run --rm node-pnpm:26 pnpm --version

# 重啟 Runner
docker restart grunner-node
```
