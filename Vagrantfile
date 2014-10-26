VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  # vagrant box add trusty-server \
  # https://cloud-images.ubuntu.com/vagrant/trusty/current/trusty-server-cloudimg-i386-vagrant-disk1.box
  config.vm.box = "trusty-server"

  EMACSEN = %w{emacs-24.4}
  SOURCE = "http://ftp.gnu.org/pub/gnu/emacs"

  GET_EMACSEN = EMACSEN.map do |e|
    <<-EOT
(if [ ! -d /usr/local/#{e} ]; \\
then curl -sS #{SOURCE}/#{e}.tar.gz > /home/vagrant/#{e}.tar.gz && \\
tar -xzf /home/vagrant/#{e}.tar.gz && \\
(cd /home/vagrant/#{e}; ./configure prefix=/usr/local/#{e}; make; make install);
fi;) \\
EOT
  end.join(" && \\")
#(if [ ! -f /root/.cask ]; then curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python; fi)

  SCRIPT = <<-EOT
sudo apt-get update && \\
sudo apt-get install -y git && \\
sudo apt-get install -y ncurses-dev && \\
#{GET_EMACSEN} && \\
PATH=/usr/local/#{EMACSEN.first}/bin/:$PATH
EOT

  config.vm.provision :shell, privileged: false, inline: SCRIPT
end
