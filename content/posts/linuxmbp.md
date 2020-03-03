---
draft: true
---

Macbook Pro 2018でArch Linuxを動かすことに成功！！！

大変だった。

Macbook Proの中にインストールもできるけど、今回はUSBにインストールした。

## 方法

1. arch linux の iso をダウンロード
1. VirtualBoxをインストール
1. VirtualBox extension pack をインストール
1. VirtualBoxで64bit仮想マシンを作成
1. USBメモリをコンピュータに挿す
1. VirtualBoxの仮想マシン設定で、ポート -> USBにいき、USB 3.0コントローラを有効にする。さらに、USBデバイスフィルターに先ほど刺したUSBメモリを登録。システム -> EFIを有効化をオンにする。
1. 仮想マシンを起動。arch linux の iso を設定する。
1. 起動したら、/etc/pacman.confに以下を追加(nanoなどで編集)

```
[mbp]
Server = https://packages.aunali1.com/archlinux/$repo/$arch
SigLevel = TrustAll
```

以降は、<https://wiki.archlinux.org/index.php/Installation_guide>に従って設定する。キーボード・タイムゾーン・言語の設定は省く。

1. `fdisk /dev/sdb`でUSBのパーティションを設定する。GUID Partition Tableで初期化し、まず最初に300MBくらいのパーティション(`/dev/sdb1`)を作成し、次に余った領域でパーティション(`/dev/sdb2`)を作成する。
1. `mkfs.fat -F 32 /dev/sdb1`
1. `mkfs.ext4 /dev/sdb2`
1. `mount /dev/sdb2 /mnt`
1. `mkdir /mnt/efi`
1. `mount /dev/sdb1 /mnt/efi`
1. `pacstrap /mnt base linux-mbp linux-firmware linux-mbp-headers`
1. `genfstab -U /mnt >> /mnt/etc/fstab`
1. `arch-chroot /mnt`
1. `passwd`でrootパスワード設定
1. `pacman -S git make`
1. `ls /lib/modules`でカーネルIDをゲット。linux-mbp-1-5.5.7みたいなやつ。
1. `git clone --branch mbp15 https://github.com/roadrunner2/macbook12-spi-driver`
1. `cd macbook12-spi-driver`
1. Makefileを編集し、`$(shell uname -r)`をカーネルIDで置き換え。
1. `make`
1. `mv apple-ibridge.ko apple-ib-tb.ko apple-ib-als.ko /lib/modules/カーネルID/`
depmod
1. `modprobe apple-ib-tb --set-version=カーネルID`
1. `modprobe apple-ib-als --set-version=カーネルID`
1. `modprobe industrialio_triggered_buffer --set-version=カーネルID`
1 cd /
1. `git clone https://github.com/MCMrARM/mbp2018-bridge-drv.git`
1. `cd mbp2018-bridge-drv`
1. Makefileを編集し、`$(shell uname -r)`をカーネルIDで置き換え。
1. `make`
1. `mv bce.ko /lib/modules/カーネルID`
depmod 
1. `modprobe bce --set-version=カーネルID`
1 cd /
1. `mkinitcpio -p linux-mbp`
1. `pacman -S grub`
1. `grub-install --target=x86_64-efi --efi-directory=efi --bootloader-id=GRUB`
1. `/efi/GRUB/grubx64.efi` を `/efi/boot/bootx64.efi`にコピー
1. `grub-mkconfig -o /boot/grub/grub.cfg`

## 参考

以下のリンクを参考にした。

<https://gist.github.com/TRPB/437f663b545d23cc8a2073253c774be3>

<https://ichelm.hatenadiary.org/entries/2014/12/09>

<http://www.silex.jp/blog/wireless/2015/08/linux.html>

## 追記
今回は3つのドライバ・カーネルパッチを導入した。主な機能は、

- linux-mbp : SSDとの通信
- macbook12-spi-driver : キーボード&Touch Barとの通信
- mbp2018-bridge-drv : T2チップとの通信

それぞれ機能がかぶさっている感じがするので、もしかすると全部を導入しなくてもいいかも。研究よろしく。

VirtualBoxでbootable USBを作るのをはやらせたい。
