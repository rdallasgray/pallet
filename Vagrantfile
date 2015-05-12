VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  # vagrant box add trusty-64 \
  # https://cloud-images.ubuntu.com/vagrant/trusty/current/trusty-server-cloudimg-amd64-vagrant-disk1.box
  config.vm.box = "trusty-64"

  EMACS_VERSIONS = %w{emacs-24.5 emacs-24.4 emacs-24.3}
  EMACS_REPO = "http://git.savannah.gnu.org/cgit/emacs.git/snapshot/"
  REQUIRED_PACKAGES = %w{git ncurses-dev automake autoconf texinfo}

  define_method :profile_export do |cmd, fn|
    config.vm.provision :shell, inline: cmd
    config.vm.provision :shell, inline: <<-EOT
echo "#{cmd}" > /etc/profile.d/#{fn}
EOT
  end

  GET_EMACS_VERSIONS = EMACS_VERSIONS.map do |e|
    <<-EOT
(if [ ! -d /usr/local/#{e} ]; \\
then echo "downloading #{e} ..." && \\
curl -sS #{EMACS_REPO}/#{e}.tar.gz > /home/vagrant/#{e}.tar.gz && \\
tar -xzf /home/vagrant/#{e}.tar.gz && \\
(cd /home/vagrant/#{e}; \\
./autogen.sh; \\
./configure prefix=/usr/local/#{e}; \\
make bootstrap; \\
sudo make install);
fi;) \\
EOT
  end.join(" && ")

  GET_EMACS_MASTER = <<-EOT
(if [ ! -d /home/vagrant/emacs-master ]; \\
then git clone --depth=1 git://git.savannah.gnu.org/emacs.git \\
/home/vagrant/emacs-master; \\
fi;) && \\
(cd /home/vagrant/emacs-master; git pull && \\
./autogen.sh; \\
./configure prefix=/usr/local/emacs-master; \\
make bootstrap; \\
sudo make install);
EOT

  GET_REQUIRED_PACKAGES = REQUIRED_PACKAGES.map do |p|
    "sudo apt-get install -y #{p}"
  end.join(" && ")

  config.vm.provision :shell, privileged: false, inline: <<-EOT
sudo apt-get update && #{GET_REQUIRED_PACKAGES}
EOT

  config.vm.provision :shell, privileged: false, inline: GET_EMACS_VERSIONS
  config.vm.provision :shell, privileged: false, inline: GET_EMACS_MASTER

  profile_export "PATH=/usr/local/#{EMACS_VERSIONS.first}/bin:\\$PATH", "current_emacs.sh"

  config.vm.provision :shell, privileged: false, inline: <<-EOT
rm -rf /home/vagrant/.cask && \\
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
EOT

  profile_export "PATH=/home/vagrant/.cask/bin:\\$PATH", "cask.sh"

  TEST_ALL = EMACS_VERSIONS.concat(%w{emacs-master}).map do |e|
    <<-EOT
echo \\"--- Running tests under #{e} ---\\" && \\
export EMACS=/usr/local/#{e}/bin/emacs && \\
(cd /vagrant; cask install && make test) \\
EOT
  end.join(" && ")

  config.vm.provision :shell, privileged: false, inline: <<-EOT
echo "#{TEST_ALL}" > /home/vagrant/test_all.sh && chmod u+x /home/vagrant/test_all.sh
EOT
end
