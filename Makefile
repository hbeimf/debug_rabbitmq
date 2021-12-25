# TMP_PATH:=/usr/local/erlang_20.3:/usr/local/erlang_20.3/bin:$(PATH)
# TMP_PATH:=/usr/local/erlang_22.3:/usr/local/erlang_22.3/bin:$(PATH)
TMP_PATH:=/usr/local/erlang_23.3:/usr/local/erlang_23.3/bin:$(PATH)

# erlang:system_info(nif_version).

export PATH=$(TMP_PATH)

PROJECT := release_debug_rabbitmq
NODENAME := debug_rabbitmq
PREVIOUS_VERSION = 0.1.0
VERSION := 0.1.0

# 名字是有讲究的哈,　不能乱起,　rabbit@maomao-VirtualBox
run:
	./rebar3 shell --name rabbit@maomao-VirtualBox --setcookie debug_rabbitmq_cookie


init:
	sudo mkdir -p /var/lib/rabbitmq/
	sudo chmod 777 /var/lib/rabbitmq/ -R
	sudo mkdir /var/log/rabbitmq
	sudo chmod 777 /var/log/rabbitmq -R
	sudo mkdir /etc/rabbitmq
	sudo chmod 777 /etc/rabbitmq -R


cc:
	rm -rf _build/default/lib

# run:
# 	./rebar3 shell --name debug_rabbitmq@127.0.0.1 --setcookie debug_rabbitmq_cookie



rel: release_c1_node1

#  rel_cluster2 rel_cluster3


rel_cluster1: release_c1_node1 release_c1_node2

release_c1_node1: 
	./rebar3 compile

# ./rebar3 release
# ./rebar3 tar
# mkdir ./$(PROJECT)_v$(VERSION)_cluster1_node1
# tar zxvf ./_build/default/rel/$(NODENAME)/$(NODENAME)-$(VERSION).tar.gz -C ./$(PROJECT)_v$(VERSION)_cluster1_node1/
# cp ./config.ini ./$(PROJECT)_v$(VERSION)_cluster1_node1/config.ini
# cp ./gw_api.config ./$(PROJECT)_v$(VERSION)_cluster1_node1/gw_api.config
# # cp ./config/vm.cluster1.node1.args ./$(PROJECT)_v$(VERSION)_cluster1_node1/releases/$(VERSION)/vm.args
# tar czvf  ./bin/$(PROJECT)_v$(VERSION)_cluster1_node1.tar.gz ./$(PROJECT)_v$(VERSION)_cluster1_node1
# rm -rf ./$(PROJECT)_v$(VERSION)_cluster1_node1

release_c1_node2: 
	./rebar3 compile
	./rebar3 release
	./rebar3 tar
	mkdir ./$(PROJECT)_v$(VERSION)_cluster1_node2
	tar zxvf ./_build/default/rel/$(NODENAME)/$(NODENAME)-$(VERSION).tar.gz -C ./$(PROJECT)_v$(VERSION)_cluster1_node2/
	cp ./config/config.cluster1.node2.ini ./$(PROJECT)_v$(VERSION)_cluster1_node2/config.ini
	cp ./gw_hub.config ./$(PROJECT)_v$(VERSION)_cluster1_node2/gw_hub.config
	cp ./config/vm.cluster1.node2.args ./$(PROJECT)_v$(VERSION)_cluster1_node2/releases/$(VERSION)/vm.args
	tar czvf  ./bin/$(PROJECT)_v$(VERSION)_cluster1_node2.tar.gz ./$(PROJECT)_v$(VERSION)_cluster1_node2
	rm -rf ./$(PROJECT)_v$(VERSION)_cluster1_node2


rel_cluster2: release_c2_node1 release_c2_node2

release_c2_node1: 
	./rebar3 compile
	./rebar3 release
	./rebar3 tar
	mkdir ./$(PROJECT)_v$(VERSION)_cluster2_node1
	tar zxvf ./_build/default/rel/$(NODENAME)/$(NODENAME)-$(VERSION).tar.gz -C ./$(PROJECT)_v$(VERSION)_cluster2_node1/
	cp ./config/config.c2.node1.ini ./$(PROJECT)_v$(VERSION)_cluster2_node1/config.ini
	cp ./config/hubs.config ./$(PROJECT)_v$(VERSION)_cluster2_node1/hubs.config
	cp ./config/vm.c2.node1.args ./$(PROJECT)_v$(VERSION)_cluster2_node1/releases/$(VERSION)/vm.args
	tar czvf  ./bin/$(PROJECT)_v$(VERSION)_cluster2_node1.tar.gz ./$(PROJECT)_v$(VERSION)_cluster2_node1
	rm -rf ./$(PROJECT)_v$(VERSION)_cluster2_node1

release_c2_node2: 
	./rebar3 compile
	./rebar3 release
	./rebar3 tar
	mkdir ./$(PROJECT)_v$(VERSION)_cluster2_node2
	tar zxvf ./_build/default/rel/$(NODENAME)/$(NODENAME)-$(VERSION).tar.gz -C ./$(PROJECT)_v$(VERSION)_cluster2_node2/
	cp ./config/config.c2.node2.ini ./$(PROJECT)_v$(VERSION)_cluster2_node2/config.ini
	cp ./config/hubs.config ./$(PROJECT)_v$(VERSION)_cluster2_node2/hubs.config
	cp ./config/vm.c2.node2.args ./$(PROJECT)_v$(VERSION)_cluster2_node2/releases/$(VERSION)/vm.args
	tar czvf  ./bin/$(PROJECT)_v$(VERSION)_cluster2_node2.tar.gz ./$(PROJECT)_v$(VERSION)_cluster2_node2
	rm -rf ./$(PROJECT)_v$(VERSION)_cluster2_node2

rel_cluster3: release_c3_node1 release_c3_node2

release_c3_node1: 
	./rebar3 compile
	./rebar3 release
	./rebar3 tar
	mkdir ./$(PROJECT)_v$(VERSION)_cluster3_node1
	tar zxvf ./_build/default/rel/$(NODENAME)/$(NODENAME)-$(VERSION).tar.gz -C ./$(PROJECT)_v$(VERSION)_cluster3_node1/
	cp ./config/config.c3.node1.ini ./$(PROJECT)_v$(VERSION)_cluster3_node1/config.ini
	cp ./config/hubs.config ./$(PROJECT)_v$(VERSION)_cluster3_node1/hubs.config
	cp ./config/vm.c3.node1.args ./$(PROJECT)_v$(VERSION)_cluster3_node1/releases/$(VERSION)/vm.args
	tar czvf  ./bin/$(PROJECT)_v$(VERSION)_cluster3_node1.tar.gz ./$(PROJECT)_v$(VERSION)_cluster3_node1
	rm -rf ./$(PROJECT)_v$(VERSION)_cluster3_node1

release_c3_node2: 
	./rebar3 compile
	./rebar3 release
	./rebar3 tar
	mkdir ./$(PROJECT)_v$(VERSION)_cluster3_node2
	tar zxvf ./_build/default/rel/$(NODENAME)/$(NODENAME)-$(VERSION).tar.gz -C ./$(PROJECT)_v$(VERSION)_cluster3_node2/
	cp ./config/config.c3.node2.ini ./$(PROJECT)_v$(VERSION)_cluster3_node2/config.ini
	cp ./config/hubs.config ./$(PROJECT)_v$(VERSION)_cluster3_node2/hubs.config
	cp ./config/vm.c3.node2.args ./$(PROJECT)_v$(VERSION)_cluster3_node2/releases/$(VERSION)/vm.args
	tar czvf  ./bin/$(PROJECT)_v$(VERSION)_cluster3_node2.tar.gz ./$(PROJECT)_v$(VERSION)_cluster3_node2
	rm -rf ./$(PROJECT)_v$(VERSION)_cluster3_node2



unzip:
	tar xzvf ./bin/release_gw_v0.1.0_cluster1_node1.tar.gz -C ./bin/
	tar xzvf ./bin/release_gw_v0.1.0_cluster1_node2.tar.gz -C ./bin/
	# tar xzvf ./bin/release_gw_v0.1.0_cluster2_node1.tar.gz -C ./bin/
	# tar xzvf ./bin/release_gw_v0.1.0_cluster2_node2.tar.gz -C ./bin/
	# tar xzvf ./bin/release_gw_v0.1.0_cluster3_node1.tar.gz -C ./bin/
	# tar xzvf ./bin/release_gw_v0.1.0_cluster3_node2.tar.gz -C ./bin/

clean:
	rm -rf ./bin/*

start: rel unzip
	./bin/release_gw_v0.1.0_cluster1_node1/bin/gw start
	./bin/release_gw_v0.1.0_cluster1_node2/bin/gw start
	./bin/release_gw_v0.1.0_cluster2_node1/bin/gw start
	./bin/release_gw_v0.1.0_cluster2_node2/bin/gw start
	./bin/release_gw_v0.1.0_cluster3_node1/bin/gw start
	./bin/release_gw_v0.1.0_cluster3_node2/bin/gw start
	
stop:
	./bin/release_gw_v0.1.0_cluster1_node1/bin/gw stop
	./bin/release_gw_v0.1.0_cluster1_node2/bin/gw stop
	./bin/release_gw_v0.1.0_cluster2_node1/bin/gw stop
	./bin/release_gw_v0.1.0_cluster2_node2/bin/gw stop
	./bin/release_gw_v0.1.0_cluster3_node1/bin/gw stop
	./bin/release_gw_v0.1.0_cluster3_node2/bin/gw stop
	rm -rf ./bin/*

release_master: 
	./rebar3 compile
	./rebar3 release
	./rebar3 tar
	mkdir ./$(PROJECT)_v$(VERSION)_master
	tar zxvf ./_build/default/rel/$(NODENAME)/$(NODENAME)-$(VERSION).tar.gz -C ./$(PROJECT)_v$(VERSION)_master/
	# cp ./config/BulletRateConfig.json ./$(PROJECT)_v$(VERSION)_master/BulletRateConfig.json
	# cp ./config/RoomTypeConfig.json ./$(PROJECT)_v$(VERSION)_master/RoomTypeConfig.json	
	cp ./config.ini ./$(PROJECT)_v$(VERSION)_master/config.ini
	cp ./hubs.config ./$(PROJECT)_v$(VERSION)_master/hubs.config
	cp ./config/vm.args ./$(PROJECT)_v$(VERSION)_master/releases/$(VERSION)/vm.args
	tar czvf  ./bin/$(PROJECT)_v$(VERSION)_master.tar.gz ./$(PROJECT)_v$(VERSION)_master 
	rm -rf ./$(PROJECT)_v$(VERSION)_master


up:
	./rebar3 release
	./rebar3 appup generate --previous_version $(PREVIOUS_VERSION)
	./rebar3 relup
	./rebar3 tar
	cp ./_build/default/rel/account_center/account_center-$(VERSION).tar.gz ./bin/
	# cd generate && python gen_release.py up $(PREVIOUS_VERSION) $(VERSION)

# c12:
# 	rm config.ini
# 	cp ./config.c12.ini ./config.ini
# 	./rebar3 shell --name c12@127.0.0.1 --setcookie c1_cookie


# c21:
# 	rm config.ini
# 	cp ./config.c21.ini ./config.ini
# 	./rebar3 shell --name c21@127.0.0.1 --setcookie c2_cookie


# c22:
# 	rm config.ini
# 	cp ./config.c22.ini ./config.ini
# 	./rebar3 shell --name c22@127.0.0.1 --setcookie c2_cookie


# c23:
# 	rm config.ini
# 	cp ./config.c23.ini ./config.ini
# 	./rebar3 shell --name c23@127.0.0.1 --setcookie c2_cookie



# stop:
# 	ps -efww|grep xgn.node|grep -v grep|cut -c 9-15|xargs kill -9

add:
	git add .
	git commit -m 'pu'

push:
	git add .
	git commit -m 'pu'
	git push

# ps:
# 	ps aux | grep xgn.node

# rel: 
# 	./rebar3 compile
# 	./rebar3 release
# 	./rebar3 tar
