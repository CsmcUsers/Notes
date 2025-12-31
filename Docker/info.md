# Nexus NuGet DNS/Connectivity 排查與修復步驟

說明：本文件針對情況 — 宿主能解析並連到 api.nuget.org，但 Nexus（Docker container）無法解析 / 連外，導致 NuGet proxy 被 Auto-Blocked。按步驟執行、驗證與回復 Nexus 服務。

---

## 前提

- Nexus container 名稱為 `nexus`（若不同請代換）。
- 你有宿主 root 權限或 sudo。
- 你可重啟 docker 及 Nexus container。

---

## 一、快速驗證（收集資訊）

1. 檢查 container 的 resolv.conf

```
docker exec nexus cat /etc/resolv.conf
```

2. 在宿主確認能解析與連線（再次驗證）

```
dig api.nuget.org +short
curl -I https://api.nuget.org/v3/index.json
```

3. 在 Nexus 的 network namespace 交互測試（可用一個工具 container）

```
docker run --rm --network container:nexus -it tutum/dnsutils /bin/sh
# 進入後執行：
dig @8.8.8.8 api.nuget.org +short
dig @10.1.1.5 api.nuget.org +short
curl -I https://api.nuget.org/v3/index.json --max-time 10
```

若 `dig @8.8.8.8` 或 `dig @10.1.1.5` timeout/無回應，表示 container(net ns) 到 DNS 的 53 port 或整體出站被阻擋。

---

## 二、判斷路徑（依驗證結果選擇）

A. 若 container 對公共 DNS（8.8.8.8）或內部 DNS（10.1.1.5）都能解析 → 問題可能已解（跳到第六步）。  
B. 若宿主能解析但 container 不能 → 問題在容器網路層（iptables/nftables/bridge/路由）或 Docker DNS 設定。  
C. 若 container 能解析 10.1.1.5 但解析遠端名稱失敗 → 內部 DNS 無法對外轉發，需網路團隊處理。

---

## 三、臨時測試（快速驗證是否能用公共 DNS）

在宿主（不用 network container mode）執行：

```
docker run --rm --dns 8.8.8.8 tutum/dnsutils dig api.nuget.org +short
```

- 若回 IP：表示宿主/daemon 可連 8.8.8.8，問題在 nexus container 的 network namespace 本身或 docker daemon 的預設 DNS 配置。
- 若仍 timeout：表示宿主環境（或雲安全組）阻擋到 8.8.8.8:53。

---

## 四、可採用的修復方案（選其一）

方案 A — 推薦：在 Docker daemon 設定全局 DNS（適用大多數情況）

1. 編輯 `/etc/docker/daemon.json`（若已有其他設定，請合併成合法 JSON）：

```
{
  "dns": ["8.8.8.8", "1.1.1.1"]
}
```

2. 重啟 Docker 並重啟 Nexus：

```
sudo systemctl restart docker
docker restart nexus
```

3. 驗證：

```
docker exec nexus cat /etc/resolv.conf
docker run --rm --network container:nexus tutum/dnsutils dig api.nuget.org +short
```

方案 B — 指定 DNS 啟動 Nexus（不變更 daemon.json）

1. 停掉並移除 nexus container：

```
docker stop nexus && docker rm nexus
```

2. 重新啟動時指定 DNS：

```
docker run -d --name nexus --dns 8.8.8.8 --dns 1.1.1.1 -p 8081:8081 -v /opt/nexus-data:/nexus-data sonatype/nexus3
```

3. 驗證同方案 A 第 3 步。

方案 C — 讓內部 DNS 可對外解析（需網路團隊）

- 若公司政策必須使用內部 DNS（如 10.1.1.5），請 IT/Network：
  - 確認 10.1.1.5 能夠轉發/解析外網域名（配置轉發器或允許遞迴）。
  - 開放容器網段到 10.1.1.5 的 UDP/TCP 53（若防火牆阻擋）。
- 驗證：

```
docker run --rm --network container:nexus tutum/dnsutils dig @10.1.1.5 api.nuget.org +short
```

方案 D — 若網路必須走 HTTP(S) Proxy

- 在啟動 Nexus container 時設定環境變數：

```
-e HTTP_PROXY=http://proxy:port -e HTTPS_PROXY=http://proxy:port
```

- 並在 Nexus JVM 內設定 proxy（若需要）。

---

## 五、檢查宿主防火牆 / 規則（若上面皆失敗）

1. 列出 iptables / nftables 規則：

```
sudo iptables -L -n -v
sudo iptables -t nat -L -n -v
sudo nft list ruleset
```

2. 如果發現 DROP/REJECT 針對 docker bridge 子網（例 172.17.0.0/16），允許容器到 DNS/HTTPS（示例）：

```
sudo iptables -I OUTPUT -p udp -s 172.17.0.0/16 --dport 53 -j ACCEPT
sudo iptables -I OUTPUT -p tcp -s 172.17.0.0/16 --dport 53 -j ACCEPT
sudo iptables -I OUTPUT -p tcp -s 172.17.0.0/16 --dport 443 -j ACCEPT
```

（請依實際 docker network subnet 調整來源）

---

## 六、解除 Nexus Auto-Block 並驗證 NuGet

1. 在 Nexus UI：Repositories → 選該 nuget proxy → 點選 Unblock / Reset / Enable outbound（名稱依版本不同）。
2. 在宿主或其他機器檢查 index.json：

```
curl -I "http://<nexus-host>:8081/repository/nuget-proxy/index.json"
```

3. 在 client 測試安裝套件：

```
dotnet nuget add source "http://<nexus-host>:8081/repository/nuget-group/" --name NexusTest
dotnet add package Newtonsoft.Json --source NexusTest
```

---

## 七、常見補充說明與建議

- 8.8.8.8 = Google Public DNS；1.1.1.1 = Cloudflare DNS。可作為替代解析器用於測試或臨時修復。
- 若公司政策不允許使用公有 DNS，請使用公司內部 DNS 並讓其開啟向外轉發。
- 優先採用方案 A（daemon.json）為長期且不破壞現有 container 行為的方式。
- 修改防火牆或 DNS 設定時請與 network/security 團隊協調，避免違反公司安全政策。

---

## 八、濃縮操作順序（執行清單）

1. docker exec nexus cat /etc/resolv.conf
2. 在宿主確認 dig/curl 可用
3. docker run --rm --network container:nexus -it tutum/dnsutils /bin/sh，做 dig @8.8.8.8 / dig @10.1.1.5 測試
4. 若容器不能解析，先嘗試方案 A：
   - 編輯 /etc/docker/daemon.json 加入 DNS
   - sudo systemctl restart docker
   - docker restart nexus
5. 若不可行，採方案 C 與網路團隊協調開放 53/443 或啟用 proxy
6. Nexus UI 解除 Auto-Block，驗證 index.json 與 dotnet add package

---

如需我產生可直接貼用的 /etc/docker/daemon.json 範例、或根據你目前的 iptables 輸出產生精確允許規則，請貼出該輸出或回覆要我產生的檔案範例。
