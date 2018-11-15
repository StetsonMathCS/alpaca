Vagrant.configure("2") do |config|
    config.vm.define "sr_range_1" do |sr_range_1|
	sr_range_1.vm.box = "ubuntu/trusty64"
	sr_range_1.vm.hostname = "sr"
	sr_range_1.vm.network :private_network, ip: "192.168.75.75"
	sr_range_1.vm.provision "ansible" do |ansible|
		ansible.playbook = "ansible/playbook.yml"
		ansible.inventory_path = "ansible/inventories/dev"
		ansible.limit = 'all'
        end
    end
end