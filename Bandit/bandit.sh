# https://overthewire.org/wargames/bandit/

# Level 0
# bandit0
ssh -p 2220 bandit0@bandit.labs.overthewire.org

# Level 1
cat readme
# NH2SXQwcBdpmTEzi3bvBHMM9H66vVXjL
ssh -p 2220 bandit1@bandit.labs.overthewire.org

# Level 2
cat ./-
# rRGizSaX8Mk1RTb1CNQoXTcYZWU6lgzi
ssh -p 2220 bandit2@bandit.labs.overthewire.org

# Level 3
cat 'spaces in this filename'
# aBZ0W5EmUfAf7kHTQeOwd8bauFJ2lAiG
ssh -p 2220 bandit3@bandit.labs.overthewire.org

# Level 4
ls -a ./inhere
cat ./inhere/.hidden
# 2EW7BBsr6aMMoJ2HjW067dm8EgX26xNe
ssh -p 2220 bandit4@bandit.labs.overthewire.org

# Level 5
cd inhere/
cat ./-file07
# lrIWWI6bB37kxfiCQZqUdOIYfr6eEeqR
ssh -p 2220 bandit5@bandit.labs.overthewire.org

# Level 6
cd inhere/
find inhere -type f -size 1033c -exec file {} \; | grep -i 'text' | grep -v 'executable'
cat inhere/maybehere07/.file2
# P4L4vucdmLnm8I7Vl7jG1ApGSfjYKqJU
ssh -p 2220 bandit6@bandit.labs.overthewire.org

# Level 7
find / -type f -user bandit7 -group bandit6 -size 33c 2>/dev/null
cat /var/lib/dpkg/info/bandit7.password
# z7WtoNQU2XfjmMtWA8u5rN4vzqu4v99S
ssh -p 2220 bandit7@bandit.labs.overthewire.org
