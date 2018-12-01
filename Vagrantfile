Vagrant.configure("2") do |config|
    config.vm.define "a" do |a|
        a.vm.box = "ubuntu/trusty64"
        a.vm.hostname = "sr"
        a.vm.network :private_network, ip: "192.168.75.75"
        a.vm.provision "ansible" do |ansible|
                ansible.playbook = "ansible/playbook.yml"
                ansible.inventory_path = "ansible/inventories/dev"
                ansible.limit = "all"
        end
    end

    config.vm.define "b" do |b|
        b.vm.box = "ubuntu/trusty64"
        b.vm.hostname = "sr"
        b.vm.network :private_network, ip: "192.168.75.76"
        b.vm.network :private_network, ip: "192.168.77.75"
        b.vm.provision "ansible" do |ansible|
                ansible.playbook = "ansible/playbook.yml"
                ansible.inventory_path = "ansible/inventories/dev"
                ansible.limit = "all"
        end
    end

    config.vm.define "c" do |c|
        c.vm.box = "ubuntu/trusty64"
        c.vm.hostname = "sr"
        c.vm.network :private_network, ip: "192.168.75.77"
        c.vm.provision "ansible" do |ansible|
                ansible.playbook = "ansible/playbook.yml"
                ansible.inventory_path = "ansible/inventories/dev"
                ansible.limit = "all"
        end
    end

    config.vm.define "d" do |d|
        d.vm.box = "ubuntu/trusty64"
        d.vm.hostname = "sr"
        d.vm.network :private_network, ip: "192.168.76.75"
        d.vm.network :private_network, ip: "192.168.77.76"
        d.vm.provision "ansible" do |ansible|
                ansible.playbook = "ansible/playbook.yml"
                ansible.inventory_path = "ansible/inventories/dev"
                ansible.limit = "all"
        end
    end

    config.vm.define "e" do |e|
        e.vm.box = "ubuntu/trusty64"
        e.vm.hostname = "sr"
        e.vm.network :private_network, ip: "192.168.76.76"
        e.vm.provision "ansible" do |ansible|
                ansible.playbook = "ansible/playbook.yml"
                ansible.inventory_path = "ansible/inventories/dev"
                ansible.limit = "all"
        end
    end
end