FROM ubuntu:22.04

COPY password.txt /

ENV DEBIAN_FRONTEND noninteractive

RUN apt update \
    && apt install -y tmux \
                      vim \
                      curl \
                      git \
                      openssh-server \
                      xauth \
                      r-base \
                      python3 \
                      python3.10-venv \
                      python3-tk \
                      python3-pip \
    && mkdir /var/run/sshd \
    && mkdir /root/.ssh \
    && chmod 700 /root/.ssh \
    && sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config \
    && sed -i "s/^.*X11Forwarding.*$/X11Forwarding yes/" /etc/ssh/sshd_config \
    && sed -i "s/^.*X11UseLocalhost.*$/X11UseLocalhost no/" /etc/ssh/sshd_config \
    && grep "^X11UseLocalhost" /etc/ssh/sshd_config || echo "X11UseLocalhost no" >> /etc/ssh/sshd_config \
    && cat /password.txt /password.txt | passwd \
    && rm /password.txt \
    && cd ~

RUN git clone https://github.com/baogorek/Miscellaneous.git \
    && cd Miscellaneous \
    && /bin/bash -c "source profile-setup.sh" \
    && cd ~

# Install Python packages
RUN python3 -m venv ~/envs/ben \
    && /bin/bash -c "source ~/envs/ben/bin/activate" \
    && pip install tensorflow pandas numpy datascroller ipython opencv-python matplotlib \
       scikit-learn


# Install R packages
RUN Rscript -e 'install.packages(c("ggplot2", "dplyr", "tidyr", "readr"))'

EXPOSE 22
ENTRYPOINT ["sh", "-c", "/usr/sbin/sshd && tail -f /dev/null"]
