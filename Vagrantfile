Vagrant.configure("2") do |config|
    config.vm.define "heyley" do |heyley|
        heyley.vm.box = "ubuntu/trusty64"
        heyley.vm.hostname = "sr"
        heyley.vm.network :private_network, ip: "192.168.75.75"
        heyley.vm.provision "ansible" do |ansible|
                ansible.playbook = "ansible/playbook.yml"
                ansible.inventory_path = "ansible/inventories/dev"
                ansible.limit = "all"
        end
    end
end