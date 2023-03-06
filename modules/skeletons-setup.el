(define-skeleton place-skeleton
  "Metadata for places to visit" nil
  ":PROPERTIES:
:LINK:
:VISITED:
:COMMENT:
:END:")

(define-skeleton challenge-skeleton
  "Headings for hacking challenges" nil
  "**** Commands\n**** Walkthrough\n**** Resources")

(define-skeleton equation-skeleton
  "Equation environment for LaTeX" nil
  "\\begin{equation}

\\end{equation}")

(define-skeleton latex-image-skeleton
  "Image environment for LaTeX documents" nil
  "#+BEGIN_CENTER
#+ATTR_LATEX: :height 0.5\\textwidth
#+CAPTION:

#+END_CENTER")

(define-skeleton exam-skeleton
  "Exam log template for org files" nil
  "- *Exam date(s)*:
- *Exam type*:
- *Feelings*:
- *Mark(s)*")

(define-skeleton pwn-ctf-skeleton
  "Python template used to solve pwn challenges" nil
  "from pwn import *

context.terminal = ['tmux', 'splitw', '-v']

if \"REMOTE\" not in args:
    r = process(\"\")
    gdb.attach(r, \"\"\"
    \"\"\")

    input(\"wait\")
else:
    r = remote(\"\", )")

(define-skeleton kernel-module-with-playground-skeleton
  "Makefile skeleton for a kernel module built in the kernel playground env" nil
  "CONFIG_MODULE_SIG=n

# module name
MODULE          := modname

# build
BUILD_DIR       := /sources/linux
MODULE_DIR      := /staging/initramfs/fs/modules

# source dirs
SRC_S           := src

# module object
obj-m           := $(MODULE).o

# core object
$(MODULE)-y     += src/core.o

# flags
ccflags-y       := -g -DDEBUG

# compilation
all: prepare
        $(MAKE) -C $(BUILD_DIR) M=$(PWD) modules

clean:
        $(MAKE) -C $(BUILD_DIR) M=$(PWD) clean

copy-to-fs: all
        mkdir -p $(MODULE_DIR)
        cp $(MODULE).ko $(MODULE_DIR)

prepare:
        cp /sources/linux/scripts/module.lds.S /sources/linux/scripts/module.lds
        sed -i '$ d' /sources/linux/scripts/module.lds")


(provide 'skeletons-setup)
