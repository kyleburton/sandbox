((nil . ((eval . (let ((root-dir (dir-locals-find-file "./")))
                   (load (expand-file-name "project.el"
                                           (if (stringp root-dir)
                                             root-dir
                                             (car root-dir)))))))))

