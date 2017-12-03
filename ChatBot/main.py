#!/usr/bin/python3.5
# encoding: utf-8


from chatterbot.trainers import ListTrainer
from chatterbot import ChatBot
import os

 #Read_only=True --> Dizer ao bot que eu já o treinei e não precisa treinar novamente.

bot = ChatBot("Teste")#read_only=True)
bot.set_trainer(ListTrainer)

for arq in os.listdir("arqs"):
	chats = open("arqs/"+arq,"rb").readlines()
	bot.train(chats)


conversa = open("arqs/conversa", "a")
while True:
	resq = input("Você: ")
	conversa.write(str(resq)+"\n")
	resp = bot.get_response(resq)
	conversa.write(str(resp)+"\n")
	print("Bot: "+ str(resp))
	if (("chega de papo" in str(resq)) or ("chega de papo" in str(resp))):
		break
conversa.close()