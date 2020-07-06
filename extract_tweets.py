import glob
import json
import string
import re
import emoji

def get_full_text(tweet):
    if 'extended_tweet' in tweet:
        return tweet['extended_tweet']['full_text']
    else:
        return tweet['text']
    
def clean_up_text(text):
    text = text.replace('\n','').replace('\r','').replace('\t','')
    while '  ' in text:
        text = text.replace('  ',' ')
    return text.strip()
        
def get_orig_text(tweet):
    if 'retweeted_status' in tweet:
        return get_orig_text(tweet['retweeted_status'])
    else:
        return clean_up_text(get_full_text(tweet))
    
def get_raw_tweets(foldername,stop):
    directory=[f for f in glob.iglob('Downloads/'+foldername+'/*')]
    raw_tweets = []
    i = 0
    msg = True
    for filepath in directory:
        file = open(filepath, 'r')
        for line in file:
            
            tweet = json.loads(line)
            
            text = get_orig_text(tweet)
            
            if ('http://' not in text) and ('https://' not in text):
                text = re.sub('@[A-Za-z0-9]+', '', text)
                text = re.sub('#', '', text)
                text = text.translate(str.maketrans('', '', string.punctuation))
                text = text.lower()
                
                raw_tweets.append(text)
                msg = True
                
                if len(raw_tweets) >= stop:
                    file.close()
                    return list(set(raw_tweets))
                
                if (stop<float('inf')) and msg:
                    if (len(raw_tweets) > 0) and (len(raw_tweets) % (stop//10) == 0):
                        print(str(len(raw_tweets))+' of '+str(stop)+' tweets read')
                    msg = False
                
        file.close()
        
        if stop==float('inf'):
            if (i > 0) and (i % (len(directory)//10) == 0):
                print(str(i)+' of '+str(len(directory))+' files read ('+
                      str(len(raw_tweets))+' total tweets)')
            
        i += 1
        
    return list(set(raw_tweets))

def clean_tweets(X):
    cleaned_tweets = []
    i = 0
    for i, origtweet in enumerate(X):
        i += 1
        if i % (len(X)//10) == 0:
            print(str(10*i/(len(X)//10))+'% read')

        emojis = []
        no_emojis = True
        for e in emoji.core.unicode_codes.UNICODE_EMOJI:
            if e in origtweet:
                tweet = ''
                no_emojis = False
                for char in origtweet:
                    if char == e:
                        tweet += ' '+char+' ' # this is so we can parse out the emoji

                        emojis.append(e)
                    else:
                        tweet += char
                origtweet = tweet
        if no_emojis:
            continue

        cleaned_tweets.append(clean_up_text(tweet))
        
    return list(set(cleaned_tweets))
    
def get__X_and__y(cleaned_tweets, emoji_sentiment):
    _X = []
    _y = []
    for i, tweet in enumerate(cleaned_tweets):
        sentiments = []
        
        not_emoji = False
        for e in emoji_sentiment:
            if 'not '+e in tweet:
                not_emoji = True
        if not_emoji: continue
            
        emojis = []
        for word in tweet.split(' '):
            for e in emoji_sentiment:
                if word == e:
                    sentiments.append(emoji_sentiment[e])
                    emojis.append(e)
        
        s = set(sentiments)
        l = len(s)
        if (l == 3) or (l == 0):
            continue
        elif l == 2:
            if 0 not in s: continue
            elif max(s) == 1: s = 1
            else: s = -1
        else:
            s = sentiments[0]
        for e in emojis:
            tweet = tweet.replace(e,'')
        _X.append(clean_up_text(tweet))
        _y.append(s)
    return _X, _y
