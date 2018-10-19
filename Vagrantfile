# -*- mode: ruby -*-
# vi: set ft=ruby :

# All Vagrant configuration is done below. The "2" in Vagrant.configure
# configures the configuration version (we support older styles for
# backwards compatibility). Please don't change it unless you know what
# you're doing.
Vagrant.configure("2") do |config|
    # The most common configuration options are documented and commented below.
    # For a complete reference, please see the online documentation at
    # https://docs.vagrantup.com.
  
    # Every Vagrant development environment requires a box. You can search for
    # boxes at https://atlas.hashicorp.com/search.
    # config.vm.box = "ubuntu/trusty64"
    # config.vm.box = "ubuntu/xenial64"
  
    config.vm.define "sr_create_range" do |sr_create_range|
	sr_create_range.vm.box = "ubuntu/trusty64"
	sr_create_range.vm.hostname = "sr-CR"
  
    #config.ssh.insert_key = false
  
    # Create a forwarded port mapping which allows access to a specific port
    # within the machine from a port on the host machine. In the example below,
    # accessing "localhost:8080" will access port 80 on the guest machine.
    # config.vm.network "forwarded_port", guest: 80, host: 8080
  
    # Create a private network, which allows host-only access to the machine
    # using a specific IP.
    # config.vm.network "private_network", ip: "192.168.33.10"
	sr_create_range.vm.network :private_network, ip: "192.168.75.75"
	sr_create_range.vm.provision "ansible" do |ansible|
		ansible.playbook = "ansible/playbook.yml"
		ansible.inventory_path = "ansible/inventories/dev"
		ansible.limit = 'all'
        end
    end
    config.vm.define "sr_create_all_paths" do |sr_create_all_paths|
    	sr_create_all_paths.vm.box = "ubuntu/trusty64"
	sr_create_all_paths.vm.hostname = "sr-CAP"
    	sr_create_all_paths.vm.network :private_network, ip: "192.168.75.76"
      #config.ssh.forward_agent = true
  
    # Share an additional folder to the guest VM. The first argument is
    # the path on the host to the actual folder. The second argument is
    # the path on the guest to mount the folder. And the optional third
    # argument is a set of non-required options.
    # config.vm.synced_folder "../data", "/vagrant_data"
  
    # Provider-specific configuration so you can fine-tune various
    # backing providers for Vagrant. These expose provider-specific options.
    # Example for VirtualBox:
    #
    # config.vm.provider "virtualbox" do |vb|
    #   # Display the VirtualBox GUI when booting the machine
    #   vb.gui = true
    #
    #   # Customize the amount of memory on the VM:
    #   vb.memory = "1024"
    # end
    #
    # View the documentation for the provider you are using for more
    # information on available options.
  
    # Need to generate ansible/playbook.yml file
    # File should only import playbook from desired directory
    	sr_create_all_paths.vm.provision "ansible" do |ansible|
        	ansible.playbook = "ansible/playbook.yml"
        	ansible.inventory_path = "ansible/inventories/dev2"
        	ansible.limit = 'all'
      	end
    end
end
  
