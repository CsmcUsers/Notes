# Docker Network

Created: 2025年3月12日 下午3:21
相關: docker
Reviewed: No

https://ithelp.ithome.com.tw/articles/10242460

# **前言**

眾所皆知，Container是個封閉的空間，但難免有些功能開發是需要與其他Containers甚至是外界串接的，因此Docker network因應不同需求發展出了多種類別的network，我們這章節就是來探討這些networks!

# **What is Docker network?**

Docker network因應不同需求分為幾種類別，分別是Bridge networks、Overlay networks、Host networking以及Macvlan networks。

# **Bridge networks**

若無更改network driver，docker network預設為Bridge networks，Bridge networks通常運用做需要獨立通信的Container當中

![](https://ithelp.ithome.com.tw/upload/images/20200923/20129737vXNEZT3NOT.png)

# **Host networking**

Host networking會使Container的隔離性質消失，在該Container當中可以直接使用例如localhost來找尋到主機上的port或其他資源。

# **Overlay networks**

Overlay networks能使不同docker daemons間互相通信，使不同群集的服務能夠交流，亦也能使不同的獨立Containers間互相通信。

![](https://ithelp.ithome.com.tw/upload/images/20200923/20129737F4jQeeTtoI.png)

# **Maclvan networks**

Maclvan允許使用者能將實體網卡設定多個mac address，並將這些address分配給Container使用，使其在network上顯示為physical address 而非 virtual address。maclvan希望能讓某些只能連到物理設備的應用程序能夠正常運作。

![](https://ithelp.ithome.com.tw/upload/images/20200923/20129737FJSrqZI9xC.png)

看完了Docker常用的4種 network strategy後，我們來講解如何使用這四種strategy吧。

# **How to use bridge networks**

# **Show network related commands**

> docker network --help
> 

```bash
$ docker network --help

Usage:	docker network COMMAND

Manage networks

Commands:
  connect     Connect a container to a network
  create      Create a network
  disconnect  Disconnect a container from a network
  inspect     Display detailed information on one or more networks
  ls          List networks
  prune       Remove all unused networks
  rm          Remove one or more networks

Run 'docker network COMMAND --help' for more information on a command.

```

# **Create bridge network**

> docker network create
> 

```bash
$ docker network create ironman-net
1b91cad2401fdf36482e1abbf47ee5cd74f1600be3556572678738623fd14991

```

# **List all networks**

> docker network ls
> 

```bash
$ docker network ls
NETWORK ID          NAME                DRIVER              SCOPE
0978533ef4d1        bridge              bridge              local
a40bb020e8b9        host                host                local
1b91cad2401f        ironman-net         bridge              local
256d21120265        none                null                local

```

# **Get more detail of network**

> docker network inspect
> 

```bash
$ docker network insepct bridge
[
    {
        "Name": "bridge",
        "Id": "653a3066b79384304599e1d343e39885ae623494bb04bbde0d8605a3aa0974a0",
        "Created": "2020-09-23T01:39:19.986816788Z",
        "Scope": "local",
        "Driver": "bridge",
        "EnableIPv6": false,
        "IPAM": {
            "Driver": "default",
            "Options": null,
            "Config": [
                {
                    "Subnet": "172.17.0.0/16",
                    "Gateway": "172.17.0.1"
                }
            ]
        },
        "Internal": false,
        "Attachable": false,
        "Ingress": false,
        "ConfigFrom": {
            "Network": ""
        },
        "ConfigOnly": false,
        "Containers": {},
        "Options": {
            "com.docker.network.bridge.default_bridge": "true",
            "com.docker.network.bridge.enable_icc": "true",
            "com.docker.network.bridge.enable_ip_masquerade": "true",
            "com.docker.network.bridge.host_binding_ipv4": "0.0.0.0",
            "com.docker.network.bridge.name": "docker0",
            "com.docker.network.driver.mtu": "1500"
        },
        "Labels": {}
    }
]

```

# **Remove bridge network**

> docker network rm
> 

```bash
$ docker network rm ironman-net
1b91cad2401fdf36482e1abbf47ee5cd74f1600be3556572678738623fd14991

```

# **Conntecting network with container**

### **Starting container with network**

```bash
$ docker run --name ironman -d --network ironman-net  -p 8100:8100 ghjjhg567/ironman:latest
c26ebd3d29bd3af588204e7d0279c0587334a64aedbf0c285a23b4634f22f9c7

```

### **Exist container connect network**

```bash
$ docker network connect ironman-net ironman

```

### **Exist container disconnect network**

```bash
$ docker network disconnect ironman-net ironman

```

### **Test two containers in the same network**

- Create two containers in the same bridge network

```bash
$ docker run --name ironman1 -dit --network ironman-net alpine ash
docker run --name ironman2 -dit --network ironman-net alpine ash

```

- Check two containers in the same network now

```bash
$ docker network inspect ironman-net
[
    {
        "Name": "ironman-net",
        "Id": "859d37a80ce2caa432ff55251f132b362af44b2b702b7caa08bd1bee6d98f797",
        "Created": "2020-09-23T02:12:05.436153605Z",
        "Scope": "local",
        "Driver": "bridge",
        "EnableIPv6": false,
        "IPAM": {
            "Driver": "default",
            "Options": {},
            "Config": [
                {
                    "Subnet": "172.19.0.0/16",
                    "Gateway": "172.19.0.1"
                }
            ]
        },
        "Internal": false,
        "Attachable": false,
        "Ingress": false,
        "ConfigFrom": {
            "Network": ""
        },
        "ConfigOnly": false,
        "Containers": {
            "4c75eb83197bda1f42e4eacc00ffcf52f451b68c46e2e530895e573c6e46e0fa": {
                "Name": "ironman1",
                "EndpointID": "83b4afdfa4781d0b62b4e153552b5d262aab0289683b9c33dd0ff41427039483",
                "MacAddress": "02:42:ac:13:00:03",
                "IPv4Address": "172.19.0.3/16",
                "IPv6Address": ""
            },
            "bc982f774a104557e2082415ca8452b2a5929b1a237f810dcf75b925030ec549": {
                "Name": "ironman2",
                "EndpointID": "3bd12690f8b7b06dbe666421f96f8df627c475ef0e22b3e1051dc7e85167fd07",
                "MacAddress": "02:42:ac:13:00:02",
                "IPv4Address": "172.19.0.2/16",
                "IPv6Address": ""
            }
        },
        "Options": {},
        "Labels": {}
    }
]

```

- Send requests from container1 to container2

```bash
$ docker exec -it ironman1 ping -c 5 ironman2
PING ironman2 (172.19.0.2): 56 data bytes
64 bytes from 172.19.0.2: seq=0 ttl=64 time=0.093 ms
64 bytes from 172.19.0.2: seq=1 ttl=64 time=0.083 ms
64 bytes from 172.19.0.2: seq=2 ttl=64 time=0.111 ms
64 bytes from 172.19.0.2: seq=3 ttl=64 time=0.121 ms
64 bytes from 172.19.0.2: seq=4 ttl=64 time=0.111 ms

--- ironman2 ping statistics ---
5 packets transmitted, 5 packets received, 0% packet loss
round-trip min/avg/max = 0.083/0.103/0.121 ms

```

透過這次的測試我們能確定，在同個bridge的containers能夠以container name當作host互通訊息。

# **How to use overlay network?**

如前言所述，overlay network主要用來連接不同docker engine所創建的network，使不同cluster中的containers能夠相互通訊。但由於本系列追求循序漸進，因此overlay network會放在後面篇章再來解說。

# **How to use host networking?**

> —network host
> 

```bash
$ docker run --name ironman -d --network host ghjjhg567/ironman:latest

```

# **How to use macvlan networks?**

- macvlan目前只支援linux/unix系統
- linxu kernal version ≥ 3.9
- promiscuous mode == True

# **Create macvlan networks**

在創建macvlan network時，需要輸入

```bash
$ docker network create -d macvlan \                                                  2526  18:14:40
  --subnet=172.16.86.0/24 \
  --gateway=172.16.86.1 \
  -o parent=eth0 ironman-net
148e412e37ef660272c407f01d9dda952e8d0abd31e62871653ee8292f213a0a

```

若想排除特定IP address，可能因為已經被使用，則需加上-aux-addresses

```bash
$ docker network create -d macvlan \                                            127 ↵  2535  18:25:35
  --subnet=172.16.86.0/24 \
  --gateway=172.16.86.1 \
  --aux-address="my-router=172.16.86.2" \
  -o parent=eth0 ironman-net
148e412e37ef660272c407f01d9dda952e8d0abd31e62871653ee8292f213a0a

```

若docker daemon允許ipv6，當然也能讓macvlan支援ivp6 protocol

```bash
$ $ docker network create -d macvlan \
    --subnet=192.168.216.0/24 --subnet=192.168.218.0/24 \
    --gateway=192.168.216.1 --gateway=192.168.218.1 \
    --subnet=2001:db8:abc8::/64 --gateway=2001:db8:abc8::10 \
     -o parent=eth0.218 \
     -o macvlan_mode=bridge macvlan216

```

# **小結**

這章節透過Docker CLI來練習除了overlaying外的其他docker networks，並讓我們知道在何種情況應該讓Container選擇使用何種Network，在後面的篇章中也會再回頭介紹Overlay network，敬請期待！