## Team Q 숫자야구 게임

## 숫자야구 코드
baseball_game <- function(gameRound=1, numberLength, Input){
  if(gameRound == 1){
    print("3자리 숫자야구를 시작합니다. 3을 눌러주세요")
    numberLength <- scan(n=1, quiet=TRUE)
    numberLength <- trunc(numberLength)
    if(numberLength != 3){
      print("잘못 입력하셨습니다. 다시 입력해주세요")
      return(baseball_game(gameRound=1, numberLength, Input))
    }
    R1 <- sample(1:9,1)
    R2 <- sample(0:9,2)
    R3 <- c(R1,R2)
    RightAnswer <- unique(R3)
    while(length(RightAnswer)==2){
      R4 <- sample(0:9,1)
      R5 <- c(RightAnswer, R4)
      RightAnswer <- unique(R5) 
    }
    Input <- RightAnswer[1:numberLength]
  }
  print("추측값 세자리를 순서대로 입력하세요")
  userInput <- scan(n=numberLength, quiet=TRUE)
  print(userInput)
  strike <- 0
  ball <- 0
  out <- 0
  strike <- sum(Input == userInput)
  for(i in 1:numberLength){
    for(j in 1:numberLength){
      if(Input[i] == userInput[j]){
        ball <- ball + 1
      }
    }
  }
  ball <- ball - strike
  out <- numberLength - strike - ball
  if(strike == numberLength){
    return(cat("","정답입니다!","\n",gameRound,"번만의 성공입니다","\n"))
  }
  if(strike != numberLength){
    gameRound <- gameRound + 1
    cat("-------------","\n", gameRound -1, "라운드 결과", "\n", strike,"Strike","\n",ball,"Ball","\n",out,"Out 입니다.","\n", "-------------", "\n")
    if(gameRound == 6){
      return(cat("", "5번의 기회를 모두 사용하셨군요", "\n", "정답은", Input, "이었습니다!", "\n", "player 패배"))
    }
    return(baseball_game(gameRound, numberLength, Input))
  }
}

## 게임 실행
baseball_game()

## 게임 초기화
rm()
