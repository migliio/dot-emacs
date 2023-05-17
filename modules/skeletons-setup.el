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

(provide 'skeletons-setup)
