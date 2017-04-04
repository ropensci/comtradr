# Fix for curl error "Peer certificate cannot be authenticated with given CA
# certificates" that keeps coming up on the Ubuntu build tests on Travis.
# For reference, see: https://www.r-bloggers.com/fixing-peer-certificate-cannot-be-authenticated/
httr::set_config(httr::config(ssl_verifypeer = 0L))
