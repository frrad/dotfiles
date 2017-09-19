Vagrant.configure("2") do |config|
  config.ssh.forward_agent = true
  config.vm.synced_folder '.', '/home/vagrant/dotfiles'

  config.vm.hostname = "devbox"

  config.vm.provider "virtualbox" do |v|
    v.name = "devbox"
    v.memory = 2048
    v.cpus = 4
  end

  config.vm.box = "debian/contrib-stretch64"

  config.vm.provision "ansible" do |ansible|
    ansible.playbook = "setup.yml"
  end

end
