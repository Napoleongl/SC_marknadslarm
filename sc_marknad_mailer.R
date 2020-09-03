suppressPackageStartupMessages(require("optparse"))
option_list <- list( 
  make_option(c("-s", "--sender"),  type="character", dest="sender",
              help="Address used to send email, must match config file."),
  make_option(c("-r", "--recipient"),  type="character", dest="recipient",
              help="Recipient of findings."),
  make_option(c("-c", "--config_file"), type="character", dest="config_file", 
              help="Gmail authorization config file path.")
)

opt <- parse_args(OptionParser(option_list=option_list))

suppressPackageStartupMessages(require("gmailr"))
source("sc_marknad_findings.R")
main <- function(sender, recipient, gm_config){
  new_ad_count  <- market_findings()
  if(new_ad_count){
    findings_table <- paste0(readLines("findings.txt"),collapse = "")
    gm_auth_configure(path=gm_config)
    gm_auth(email = sender, cache = ".secret")
    gm_mime() %>% 
      gm_to(recipient) %>% 
      gm_from(sender) %>% 
      gm_subject("Nya grejor till salu!") %>% 
      gm_html_body(findings_table) %>% 
      gm_send_message()
    write(paste("Mail sent with", new_ad_count ,"new ads."), "")
  } else {
    write("No new findings!", "")
  }
}

main(opt$sender, opt$recipient, opt$config_file)
