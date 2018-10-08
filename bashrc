#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

export TERM=xterm-256color
export VISUAL=vim
export EDITOR=vim

alias ls='ls --color=auto'
alias grep="grep --color=auto"
function tpdf() {
  pdftotext $1 /dev/stdout | less
}

# Source other files
source ~/.secrets

# Modify Paths
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/cuda:/opt/cuda/lib64:/opt/cuda/extras/CUPTI/lib64
export CUDA_HOME=/opt/cuda

export PYTHONPATH=$PYTHONPATH:/home/entilzha/code/qb
export PYTHONPATH=$PYTHONPATH:/home/entilzha/software/spark-2.2.0-bin-hadoop2.7/python
export PYTHONPATH=$PYTHONPATH:/home/entilzha/applications/pycharm-2017.1.2/debug-eggs/pycharm-debug.egg

export PATH=$PATH:/home/entilzha/.cargo/bin:/home/entilzha/bin

# Application Specific Configs
export QB_ROOT=/home/entilzha/code/qb
export QB_SPARK_MASTER=spark://nibel:7077
export QB_AWS_S3_BUCKET="entilzha-us-west-2"
export QB_AWS_S3_NAMESPACE="atlanta-word-expo"

export TF_VAR_key_pair="pedro-key"
export TF_VAR_qb_aws_s3_bucket="entilzha-us-west-2"
export TF_VAR_qb_aws_s3_namespace="atlanta-word-expo"

# SSH Agent
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent | head -n 2 > ~/.ssh-agent-thing
fi
if [[ "$SSH_AGENT_PID" == "" ]]; then
    eval "$(<~/.ssh-agent-thing)"
fi

ssh-add ~/.ssh/pedro-key.pem 2> /dev/null

exec fish

