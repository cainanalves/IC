#import speech_recognition as sr
#r = sr.Recognizer()
#
#with sr.Microphone() as s:
#	r.adjust_for_ambient_noise(s)
#
#while True:
#	audio = r.listen(s)
#	resp = bot.get_response(r.recognize_google(audio, language='pt'))
#	print('Bot: '+ str(resp))

from chatterbot.trainers	import ListTrainer
from chatterbot import ChatBot
import os

 #Read_only=True --> Dizer ao bot que eu já o treinei e não precisa treinar novamente.

bot = ChatBot('Teste') #read_only=True)

bot.set_trainer(ListTrainer)

for arq in os.listdir('arqs'):
	chats = open('arqs/'+arq,'r').readlines()
	bot.train(chats)

while True:
	resq = input('Você: ')
	resp = bot.get_response(resq)
	print('Bot: '+ str(resp))

