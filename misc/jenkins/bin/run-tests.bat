@echo off
cd /d d:\

set TARGET=gnupg-test

set GNUPGHOME=c:/%TARGET%/tests/openpgp
c:/%TARGET%/gpg-connect-agent.exe killagent /bye
rem is there a nicer way to sleep?
ping -n 1 localhost > nul
set GNUPGHOME=

rmdir /q /s c:\%TARGET%
mkdir c:\%TARGET%
xcopy /q /s d:\gnupg c:\%TARGET%

set GPGSCM_PATH=c:/%TARGET%/tests/gpgscm;c:/%TARGET%/tests/openpgp
set EXEEXT=.exe
set srcdir=/%TARGET%/tests/openpgp
set BIN_PREFIX=c:/%TARGET%
set TMP=c:\temp
mkdir %TMP%

cd /d c:\%TARGET%
c:\%TARGET%\gpgscm.exe --verbose tests/gpgscm/t-child.scm

cd /d c:\%TARGET%\tests\openpgp
c:\%TARGET%\gpgscm.exe run-tests.scm --shared version.scm mds.scm decrypt.scm decrypt-dsa.scm sigs.scm sigs-dsa.scm encrypt.scm encrypt-dsa.scm seat.scm clearsig.scm encryptp.scm detach.scm detachm.scm armsigs.scm armencrypt.scm armencryptp.scm signencrypt.scm signencrypt-dsa.scm armsignencrypt.scm armdetach.scm armdetachm.scm genkey1024.scm conventional.scm conventional-mdc.scm multisig.scm verify.scm gpgv-forged-keyring.scm armor.scm import.scm ecc.scm 4gb-packet.scm tofu.scm gpgtar.scm use-exact-key.scm default-key.scm export.scm ssh.scm quick-key-manipulation.scm issue2015.scm issue2346.scm issue2417.scm issue2419.scm
