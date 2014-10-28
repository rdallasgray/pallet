VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  # vagrant box add trusty-server \
  # https://cloud-images.ubuntu.com/vagrant/trusty/current/trusty-server-cloudimg-i386-vagrant-disk1.box
  config.vm.box = "trusty-server"

  EMACSEN = %w{emacs-24.4 emacs-24.3 emacs-24.2}
  EMACS_REPO = "http://ftp.gnu.org/pub/gnu/emacs"

  define_method :profile_export do |cmd, fn|
    config.vm.provision :shell, inline: cmd
    config.vm.provision :shell, inline: <<-EOT
echo "#{cmd}" > /etc/profile.d/#{fn}
EOT
  end

  GET_EMACSEN = EMACSEN.map do |e|
    <<-EOT
(if [ ! -d /usr/local/#{e} ]; \\
then echo "downloading #{e} ..." && \\
curl -sS #{EMACS_REPO}/#{e}.tar.gz > /home/vagrant/#{e}.tar.gz && \\
tar -xzf /home/vagrant/#{e}.tar.gz && \\
(cd /home/vagrant/#{e}; ./configure prefix=/usr/local/#{e}; make; sudo make install);
fi;) \\
EOT
  end.join(" && ")

  config.vm.provision :shell, privileged: false, inline: <<-EOT
sudo apt-get update && \\
sudo apt-get install -y git && \\
sudo apt-get install -y ncurses-dev
EOT

  config.vm.provision :shell, privileged: false, inline: GET_EMACSEN

  profile_export "PATH=/usr/local/#{EMACSEN.first}/bin:$PATH", "current_emacs.sh"

  config.vm.provision :shell, privileged: false, inline: <<-EOT
rm -rf /home/vagrant/.cask && \\
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
EOT

  profile_export "export PATH=/home/vagrant/.cask/bin:$PATH", "cask.sh"

  TEST_ALL = EMACSEN.map do |e|
    <<-EOT
echo "--- Running tests under #{e} ---" && \\
EMACS=/usr/local/#{e}/bin/emacs && (cd /vagrant; cask install; make test)
EOT
  end.join(" && \\")

  config.vm.provision :shell, privileged: false, inline: <<-EOT
echo "#{TEST_ALL}" > /home/vagrant/test_all.sh && chmod u+x /home/vagrant/test_all.sh
EOT
end
