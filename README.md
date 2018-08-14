##Trends in Trust  

Looking at data from GSS   

## Where this came from
Source: Idea behind this project is to estimate trends in trust and what is correlated with them. Idea came from this twitter thread from Noah Smith as a retort to Douthat's nyt piece about immigration.

nyt articile that spurred idea:
https://www.nytimes.com/2018/01/27/opinion/immigration-stephen-miller.html

twitter thread:
https://twitter.com/Noahpinion/status/957687778589863936

##Goal:
make interactive plots of the trust battery from GSS of American's trust in society cut by different subgroups.   

##What I've done
Progress: I have ran all the weighted estimates from 72-16.

## Left to do
Settle on a suitable way of displaying the output. Most likely a shiny page, but need to setup a master site with hugo/github that should link to this portion.

##GSS variables
GSS variables; trend years   
- trust; (72-16) Generally speaking, would you say that most people can be trusted or that you can't be too careful in dealing with people?     
- fair; (72-16 (some missing)); 85. Do you think most people would try to take advantage of you if they got a chance, or would they try to be fair?  
- helpful; (72-16); Would you say that most of the time people try to be helpful, or that they are mostly just looking out for themselves?    

-institution trust  
  I am going to name some institutions in this country. As far as the people running these institutions are concerned, would you say you have a great deal of confidence, only some confidence, or hardly any confidence at all in them?  
    #A.  Banks and financial institutions [VAR: CONFINAN]   
    #B. Major companies [VAR: CONBUS]   
    #C. Organized religion [VAR: CONCLERG]  
    #D. Education [VAR: CONEDUC]  
    #E. Executive branch of the federal government [VAR: CONFED]  
    #F. Organized labor [VAR: CONLABOR]  
    #G. Press [VAR: CONPRESS]  
    #H. Medicine [VAR: CONMEDIC]  
    #I. TV [VAR: CONTV]  
    #J. U.S. Supreme Court [VAR: CONJUDGE]  
    #K. Scientific Community [VAR: CONSCI]  
    #L. Congress [VAR: CONLEGIS]  
    #M. Military [VAR: CONARMY]   