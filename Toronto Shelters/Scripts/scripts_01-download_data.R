#Importing the data:
install.packages("opendatatoronto")
library("opendatatoronto")
my_data=search_packages("Toronto Shelter System Flow") %>% 
  list_package_resources() %>% 
  filter(name=="toronto-shelter-system-flow.csv") %>% 
  get_resource()
