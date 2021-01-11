# A script providing a function to notify me when a model is done fitting
library(sendmailR)

notify_me <- function(model_name) {
	sendmail(from = 'k.trutmann@unibas.ch',
		to = 'k.trutmann@unibas.ch',
		subject = str_c('Model ', model_name, ' is done fitting!'),
   		msg = 'It\'s about time!',
   		control = list(smtpServer = 'smtp.unibas.ch', port = 25))
}