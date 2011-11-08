(in-package :burning-filesystem-test)

(defcase vfs-test)

(defun make-dp (host device path &optional (filesystem nil))
  (make-directory-path :host host :device device :path path :filesystem filesystem))

(defun make-fp (directory name type version)
  (make-file-path :directory directory :name name :type type :version version))

(deftest vfs-test basic-string-to-path ()
  (let ((fs (make-virtual-filesystem)))
    (!equalp (fs-path-from-string fs "bla")
	     (make-fp (make-dp nil nil '(:relative)) "bla" nil nil))
    (!equalp (fs-path-from-string fs "/bla-bla")
	     (make-fp (make-dp nil nil '(:absolute)) "bla-bla" nil nil))
    (!equalp (fs-path-from-string fs "/bla/bla-bla/bla-bla-bla")
	     (make-fp (make-dp nil nil '(:absolute "bla" "bla-bla")) "bla-bla-bla" nil nil))
    (!equalp (fs-path-from-string fs "bla/bla-bla/bla-bla-bla")
	     (make-fp (make-dp nil nil '(:relative "bla" "bla-bla")) "bla-bla-bla" nil nil))
    (!equalp (fs-path-from-string fs "bla/bla-bla/bla-bla-bla/")
	     (make-dp nil nil '(:relative "bla" "bla-bla" "bla-bla-bla")))
    (!equalp (fs-path-from-string fs "")
	     (make-dp nil nil '(:relative)))
    (!equalp (fs-path-from-string fs "/")
	     (make-dp nil nil '(:absolute)))))

(deftest vfs-test typed-string-to-path ()
  (let ((fs (make-virtual-filesystem)))
    (!equalp (fs-path-from-string fs "bla.bla-bla")
	     (make-fp (make-dp nil nil '(:relative)) "bla" "bla-bla" nil))
    (!equalp (fs-path-from-string fs ".bla-bla")
	     (make-fp (make-dp nil nil '(:relative)) ".bla-bla" nil nil))
    (!equalp (fs-path-from-string fs "bla.bla.bla")
	     (make-fp (make-dp nil nil '(:relative)) "bla.bla" "bla" nil))))

(deftest vfs-test parse-host-and-device ()
  (let ((fs (make-virtual-filesystem)))
    (!equalp (fs-path-from-string fs "home@/bla")
	     (make-fp (make-dp "home" nil '(:absolute)) "bla" nil nil))
    (!equalp (fs-path-from-string fs "home@/bla/")
	     (make-dp "home" nil '(:absolute "bla")))
    (!equalp (fs-path-from-string fs "home:/bla")
	     (make-fp (make-dp nil "home" '(:absolute)) "bla" nil nil))
    (!equalp (fs-path-from-string fs "home@drive:/bla/bla")
	     (make-fp (make-dp "home" "drive" '(:absolute "bla")) "bla" nil nil))
    (!equalp (fs-path-from-string fs "home@drive:/bla/bla/")
	     (make-dp "home" "drive" '(:absolute "bla" "bla")))))

(deftest vfs-test parse-version ()
  (let ((fs (make-virtual-filesystem)))
    (!equalp (fs-path-from-string fs "/bla/bla.bla:2.3.4")
	     (make-fp (make-dp nil nil '(:absolute "bla")) "bla" "bla" "2.3.4"))
    (!equalp (fs-path-from-string fs "home-host@uberdrive:/bla/bla-bla/b/b/b.b.b.b:1.2.3.4.5")
	     (make-fp (make-dp "home-host" "uberdrive" '(:absolute "bla" "bla-bla" "b" "b")) 
		      "b.b.b" "b" "1.2.3.4.5"))))

(deftest vfs-test string-from-path ()
  (let ((fs (make-virtual-filesystem)))
    (let ((path (make-fp (make-dp "host" "device" '(:absolute "dir1" "dir2")) "name1.name2" "name3" "generic")))
      (!equal (fs-path-to-string fs path) "host@device:/dir1/dir2/name1.name2.name3:generic"))
    (let ((path (make-fp (make-dp nil nil '(:relative "dir1" "dir2")) "name1" "name2" "sometime")))
      (!equal (fs-path-to-string fs path) "dir1/dir2/name1.name2:sometime"))
    (let ((path (make-dp "some-host" "some-device" '(:absolute "dir"))))
      (!equal (fs-path-to-string fs path) "some-host@some-device:/dir/"))))

(deftest vfs-test default-directory-listing ()
  (let ((fs (make-virtual-filesystem)))
    (!equalp (fs-list-directory fs (fs-path-from-string fs "/")) 
	     (list (fs-path-from-string fs "/home/")
		   (fs-path-from-string fs "/work/")))
    (!equalp (fs-list-directory fs (fs-path-from-string fs "/home/"))
	     ())
    (!equalp (fs-list-directory fs (fs-path-from-string fs "/work/"))
	     ())
    (!equalp (fs-list-directory fs (fs-path-from-string fs ""))
	     ())))

(deftest vfs-test directory-making ()
  (let ((fs (make-virtual-filesystem)))
    (fs-make-directory fs (fs-path-from-string fs "/tmp/"))
    (!equalp (fs-list-directory fs (fs-path-from-string fs "/"))
	     (list (fs-path-from-string fs "/home/")
		   (fs-path-from-string fs "/tmp/")
		   (fs-path-from-string fs "/work/"))))
  (let ((fs (make-virtual-filesystem)))
    (fs-make-directory fs (fs-path-from-string fs "bla/"))
    (!equalp (fs-list-directory fs (fs-path-from-string fs ""))
	     (list (fs-path-from-string fs "bla/")))
    (!equalp (fs-list-directory fs (fs-path-from-string fs "/work/"))
	     (list (fs-path-from-string fs "/work/bla/")))))

(deftest vfs-test home-and-current-directory ()
  (let ((fs (make-virtual-filesystem)))
    (!equalp (fs-home-directory fs) (fs-path-from-string fs "/home/"))
    (!equalp (fs-current-directory fs) (fs-path-from-string fs "/work/"))))

(deftest vfs-test creating-files ()
  (let ((fs (make-virtual-filesystem)))
    (fs-make-file fs (fs-path-from-string fs "/simple"))
    (!equalp (fs-list-directory fs (fs-path-from-string fs "/"))
	     (list (fs-path-from-string fs "/home/")
		   (fs-path-from-string fs "/work/")
		   (fs-path-from-string fs "/simple"))))
  (let ((fs (make-virtual-filesystem)))
    (fs-make-file fs (fs-path-from-string fs "/bcd"))
    (fs-make-file fs (fs-path-from-string fs "/bce"))
    (fs-make-file fs (fs-path-from-string fs "/abc"))
    (!equalp (fs-list-directory fs (fs-path-from-string fs "/"))
	     (list (fs-path-from-string fs "/home/")
		   (fs-path-from-string fs "/work/")
		   (fs-path-from-string fs "/abc")
		   (fs-path-from-string fs "/bcd")
		   (fs-path-from-string fs "/bce"))))
  (let ((fs (make-virtual-filesystem)))
    (fs-make-file fs (fs-path-from-string fs "/home/file.ext:v123"))
    (!equalp (fs-list-directory fs (fs-path-from-string fs "/home/"))
	     (list (fs-path-from-string fs "/home/file.ext:v123")))))

(deftest vfs-test deleting-directories ()
  (let ((fs (make-virtual-filesystem)))
    (fs-delete-directory fs (fs-path-from-string fs "/home/"))
    (!equalp (fs-list-directory fs (fs-path-from-string fs "/"))
	     (list (fs-path-from-string fs "/work/"))))
  (let ((fs (make-virtual-filesystem)))
    (fs-make-directory fs (fs-path-from-string fs "/home/tmp/"))
    (fs-make-directory fs (fs-path-from-string fs "/home/tmp/tmp2/"))
    (fs-delete-directory fs (fs-path-from-string fs "/home/tmp/tmp2/"))
    (!equalp (fs-list-directory fs (fs-path-from-string fs "/home/tmp/"))
	     ())
    (fs-delete-directory fs (fs-path-from-string fs "/home/tmp/"))
    (!equalp (fs-list-directory fs (fs-path-from-string fs "/home/"))
	     ())))

(deftest vfs-test deleting-files ()
  (let ((fs (make-virtual-filesystem)))
    (fs-make-directory fs (fs-path-from-string fs "tmp/"))
    (fs-make-file fs (fs-path-from-string fs "tmp/bla.bla:bla"))
    (fs-make-file fs (fs-path-from-string fs "tmp/bla.bla:new"))
    (fs-delete-file fs (fs-path-from-string fs "tmp/bla.bla:bla"))
    (!equalp (fs-list-directory fs (fs-path-from-string fs "tmp/"))
	     (list (fs-path-from-string fs "tmp/bla.bla:new")))))


  
