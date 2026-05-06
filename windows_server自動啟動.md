python?code_reference&code_event_index=3
# Content for the Markdown file
md_content = """# Windows Server Nginx 服務化配置指南

本文件整理了在 Windows Server 上將 Nginx 包裝為系統服務的完整流程、常見問題排查及後續管理建議。

---

## 1. 核心工具選擇：WinSW (Windows Service Wrapper)
微軟官方與社群廣泛推薦的工具，基於 .NET 寫成，適合企業環境。

### 安裝步驟
1. **下載：** 從 [WinSW GitHub](https://github.com/winsw/winsw/releases) 下載執行檔 (例如 `WinSW-x64.exe`)。
2. **重新命名：** 將執行檔移動到 Nginx 根目錄（如 `C:\\nginx`），並更名為 `nginx-service.exe`。
3. **建立設定檔：** 在同目錄建立 `nginx-service.xml`。

### XML 設定範例
```xml
<service>
  <id>nginx</id>
  <name>Nginx Service</name>
  <description>Nginx Web Server</description>
  <executable>C:\\nginx\\nginx.exe</executable>
  <logmode>roll</logmode>
  <stopparentprocessfirst>true</stopparentprocessfirst>
</service>
```
> **注意：** 執行檔與 XML 檔名必須完全一致，否則會報錯 `Root element is missing`。

---

## 2. 常用操作指令 (PowerShell 管理員模式)

| 功能 | 指令 |
| :--- | :--- |
| **安裝服務** | `.\\nginx-service.exe install` |
| **啟動服務** | `Start-Service nginx` |
| **停止服務** | `Stop-Service nginx` |
| **重新啟動** | `Restart-Service nginx` |
| **解除安裝** | `.\\nginx-service.exe uninstall` |
| **強制刪除服務** | `sc.exe delete nginx` |

---

## 3. 自動啟動與穩定性設定
確保伺服器重新啟動後，Nginx 能自動恢復運行。

### 確認自動啟動狀態
```powershell
Get-Service nginx | Select-Object Name, StartType, Status
```
若非自動，請設定：
```powershell
Set-Service nginx -StartupType Automatic
```

### 服務自動恢復設定 (Recovery)
1. 開啟 `services.msc`。
2. 找到 **Nginx Service** -> 內容 -> **修復** 頁籤。
3. 將「第一次、第二次及後續失敗」皆設定為 **「重新啟動服務」**。

---

## 4. 常見問題排查 (Troubleshooting)

### 錯誤：`Root element is missing`
* **原因：** WinSW 找不到 XML 檔，或 XML 內容為空/格式錯誤。
* **解法：** 確認 `nginx-service.xml` 與執行檔同名，且編碼為 **UTF-8**。

### 檢查服務是否真的啟動
```powershell
Test-NetConnection -ComputerName localhost -Port 80
```
若 `TcpTestSucceeded` 為 **True**，代表 Nginx 正常監聽中。

### 檢視系統日誌
* 開啟 **事件檢視器 (Event Viewer)** -> Windows 記錄 -> 應用程式。
* 尋找來源為 `WinSW` 或 `nginx` 的紀錄。

---

## 5. 微軟官方建議最佳實踐
1. **權限控管：** 盡量使用專用的服務帳戶執行，而非 `LocalSystem`。
2. **日誌管理：** 搭配 WinSW 的 `<logmode>roll</logmode>` 避免單一日誌檔過大。
3. **防火牆：** 確認 Windows 防火牆已開啟 80/443 埠口。



