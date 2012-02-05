# see: http://ubuntuforums.org/showthread.php?t=751169
. common.sh
cd software
ensure_dir mbrola_tmp
cd mbrola_tmp/

download http://tcts.fpms.ac.be/synthesis/mbrola/bin/pclinux/mbrola3.0.1h_i386.deb

download http://tcts.fpms.ac.be/synthesis/mbrola/dba/us1/us1-980512.zip
download http://tcts.fpms.ac.be/synthesis/mbrola/dba/us2/us2-980812.zip
download http://tcts.fpms.ac.be/synthesis/mbrola/dba/us3/us3-990208.zip

download http://www.festvox.org/packed/festival/1.96/festvox_us1.tar.gz
download http://www.festvox.org/packed/festival/1.96/festvox_us2.tar.gz
download http://www.festvox.org/packed/festival/1.96/festvox_us3.tar.gz

sudo dpkg -i mbrola3.0.1h_i386.deb

unzip_unless us1 us1-980512.zip
unzip_unless us2 us2-980812.zip
unzip_unless us3 us3-990208.zip
tar xzf festvox_us1.tar.gz
tar xzf festvox_us2.tar.gz
tar xzf festvox_us3.tar.gz


sudo mkdir -p /usr/share/festival/voices/english/us1_mbrola/
sudo mkdir -p /usr/share/festival/voices/english/us2_mbrola/
sudo mkdir -p /usr/share/festival/voices/english/us3_mbrola/



sudo mv us1 /usr/share/festival/voices/english/us1_mbrola/
sudo mv us2 /usr/share/festival/voices/english/us2_mbrola/
sudo mv us3 /usr/share/festival/voices/english/us3_mbrola/
sudo mv festival/lib/voices/english/us1_mbrola/* /usr/share/festival/voices/english/us1_mbrola/
sudo mv festival/lib/voices/english/us2_mbrola/* /usr/share/festival/voices/english/us2_mbrola/
sudo mv festival/lib/voices/english/us3_mbrola/* /usr/share/festival/voices/english/us3_mbrola/
