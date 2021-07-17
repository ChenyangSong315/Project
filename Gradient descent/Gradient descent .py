#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import numpy as np
import matplotlib.pyplot as plt 

print("Function Number:")
print("Sphere:     1")
print("Rosenbrock: 2")
print("Camel:      3")
Function = input("Please type the Function Number:")
Function = float(Function)


# binary search used to find the initial step-size
def bin_search(alpha_min,alpha_max,x0,y0):
    num=0
    while num<1000: # aviod infinite interation
        num += 1        
        # find the upper bound of f(x,y) after go one-step
        x = x0 - alpha_max * dx(x0,y0)
        y = y0 - alpha_max * dy(x0,y0)
        F_max = f(x,y) 
        
        x = x0 - alpha_min * dx(x0,y0)
        y = y0 - alpha_min * dy(x0,y0)
        F_min = f(x,y) 
        
        # find the mid value of f(x,y)
        alpha_mid = (alpha_min + alpha_max)/2
        x = x0 - alpha_mid * dx(x0,y0)
        y = y0 - alpha_mid * dy(x0,y0)
        F_mid= f(x,y)

        # reset the a norrow range    
        if min(F_max,F_mid,F_mid) == F_max:
            alpha_min = alpha_mid
        elif min(F_max,F_mid,F_mid) == F_min:
            alpha_max = alpha_mid
        elif min(F_max,F_mid,F_mid) == F_mid and F_max < F_min :
            alpha_min = alpha_mid
        elif min(F_max,F_mid,F_mid) == F_mid and F_max > F_min :
            alpha_max = alpha_mid
        else:
            alpha = alpha_mid
            return alpha
            break
        # aviod over the max numnber of iteration
        alpha = alpha_mid
        return alpha
      
####################### Sphere Function ############
if Function == 1: 
    print("\n######  Sphere Function  ######")
    def f(x,y):
        return x**2 + y**2

    def dx(x,y):
        return 2*x
    
    def dy(x,y):
        return 2*y
# staratd method
    GD_X = []
    GD_Y = []
    GD_Z = []
    # define the initial value of X and Y
    x = 3
    y = 3
    print(("\nThe initial location: x = %.5f, y = %.5f")%(x,y))
    # defien step-size by using binary_search
    alpha = bin_search(alpha_min=0.2, alpha_max=0.9, x0=3, y0=3)
    print("The initial step-size: %.5f" %alpha)
    # set the initial value of Z
    f_change = f(x,y) # record the Z value
    f_current = f_change # set the initial value of Z
    
    #record the value 
    GD_X.append(x)
    GD_Y.append(y)
    GD_Z.append(f_current)
    
# gradient descent process
    iter_num=0
    while f_change > 1e-10 and iter_num <20000:
        iter_num +=1
        pre_x = x
        pre_y = y
        x = x - alpha * dx(pre_x,pre_y)
        y = y - alpha * dy(pre_x,pre_y)
        tmp = f(x,y)
        f_change = np.abs(f_current- tmp)
        f_current = tmp
        GD_X.append(x)
        GD_Y.append(y)
        GD_Z.append(f_current)
    # -1, beacuse our condition is f_change
    # so if last iteration statisfys the condition,f_change < 1e-10
    # it is mean that before this step, we already find the local minimum
    # For this reason, in fact we just need iter_num-1's steps
    steps = iter_num-1
    print("\nStandard")
    print(("x = %.5f,y = %.5f,f(x,y)= %.5f") %(x,y,f_current))
    print("Number of steps： %d" %steps)

    
    # parameters for plot
    fig = plt.figure(figsize = (16,8))
    x = np.linspace(-5,5,250)
    y = np.linspace(-5,5,250)
    X, Y = np.meshgrid(x, y)
    Z = f(X, Y)
    
    #Surface plot
    ax = fig.add_subplot(1, 2, 1, projection='3d')
    ax.plot_surface(X,Y,Z,rstride = 5, cstride = 5, cmap = 'rainbow', alpha = .4)
    ax.plot(GD_X,GD_Y, GD_Z,color = 'b', marker = '*', alpha = .6)        
    ax.view_init(45, 240)
    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_zlabel('z')
    
    #Contour plot
    ax = fig.add_subplot(1, 2, 2)
    C = ax.contour(X,Y,Z,30,colors="K")
    ax.clabel(C, fontsize=9, inline=1)
    ax.contourf(X,Y,Z,30)
    ax.plot(GD_X,GD_Y,marker="*",color="r")

################################ Rosenbrock Function #############
elif Function == 2:
    print("\n######  Rosenbrock Function  ######")
    def f(x,y):
        return (1-x)**2 + 100 *((y-x*x)**2)

    def dx(x,y):
       return -2*(1-x)-400*x*(y-x*x)

    def dy(x,y):
        return 200*(y-x*x)
  
################################ Rosenbrock Function- Standard ############# 
    # save value of X，Y,Z
    GD_X = []
    GD_Y = []
    GD_Z = []
    # set the initial value of X and Y
    x = 2.5
    y = 5
    print(("\nThe initial location: x = %.5f, y = %.5f")%(x,y))
    
    # in order to compare with other method, we norrow the range. 
    # if we choose larger range, we cannor get return form Neserov Accelerated Gradient
    alpha=bin_search(alpha_min=0.00001, alpha_max=0.00085, x0=2.5, y0=5)
    print("The initial step-size: %.5f" %alpha)
    # set the initial value of f(x,y)
    f_change = f(x,y) # record the (x,y) value
    f_current = f_change # set the initial value of (x,y)
    
    #record the value 
    GD_X.append(x)
    GD_Y.append(y)
    GD_Z.append(f_current)
    #interation 
    iter_num=0
   
    while f_change > 1e-10 and iter_num < 200000:
        iter_num +=1
        prex = x
        prey = y
        x = x - alpha * dx(prex,prey)
        y = y - alpha * dy(prex,prey)
    
        tmp = f(x,y)
        f_change = np.abs(f_current- tmp)
        f_current = tmp
        GD_X.append(x)
        GD_Y.append(y)
        GD_Z.append(f_current)
    steps = iter_num-1
    print("\nStandard")
    print(("x = %.5f,y = %.5f,f(x,y)= %.5f") %(x,y,f_current))
    print("Number of steps： %d" %steps)


    fig = plt.figure(figsize = (16,8))
    x = np.linspace(-3,3,250)
    y = np.linspace(-4,7,250)
    X, Y = np.meshgrid(x, y)
    Z = f(X, Y)

    
    #Surface plot
    ax = fig.add_subplot(1, 2, 1, projection='3d')
    ax.plot_surface(X,Y,Z,rstride = 5, cstride = 5, cmap = 'rainbow', alpha = .4)
    ax.plot(GD_X,GD_Y, GD_Z,color = 'b', marker = '*', alpha = .6)
    
    ax.view_init(45, 240)
    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_zlabel('z')
    
    #Contour plot
    ax = fig.add_subplot(1, 2, 2)
    C =ax.contour(X,Y,Z,20,colors="K")
    ax.clabel(C, fontsize=9, inline=1)
    ax.contourf(X,Y,Z,20)    
    ax.plot(GD_X,GD_Y,marker="*",color="r") # point and path    
    ax.set_title('Gradient Descent with {} iterations'.format(iter_num))    
    plt.show()


################################ Rosenbrock Function - Momentum ##############

     # save value of X，Y,Z
    GD_X = []
    GD_Y = []
    GD_Z = []
    # set the initial value of X and Y
    x = 2.5
    y = 5
    Vx = 0 #initial velociay
    Vy = 0
    alpha=bin_search(alpha_min=0.0001, alpha_max=0.00085, x0=2.5, y0=5)
    # set the initial value of Z
    f_change = f(x,y) # record the Z value
    f_current = f_change # set the initial value of Z
    
    #record the value 
    GD_X.append(x)
    GD_Y.append(y)
    GD_Z.append(f_current)
    
    #interation 
    iter_num=0
    while f_change > 1e-10 and iter_num < 20000:
        iter_num +=1
        pre_x = x
        pre_y = y
        pre_Vx = Vx
        pre_Vy= Vy 
    
        Vx = 0.9*pre_Vx + alpha * dx(pre_x,pre_y)
        Vy = 0.9*pre_Vy + alpha * dy(pre_x,pre_y)
        x = pre_x - Vx
        y = pre_y - Vy
    
        tmp = f(x,y)
        f_change = np.abs(f_current - tmp)
        f_current = tmp
        GD_X.append(x)
        GD_Y.append(y)
        GD_Z.append(f_current)
    steps = iter_num-1
    print("\nMomentum")
    print(("x = %.5f,y = %.5f,f(x,y)= %.5f") %(x,y,f_current))
    print("Number of steps： %d" %steps )
    
    
    fig = plt.figure(figsize = (16,8))
    x = np.linspace(-3.1,3.1,250)
    y = np.linspace(-4,7,250)
    X, Y = np.meshgrid(x, y)
    Z = f(X, Y)
    
    #Surface plot
    ax = fig.add_subplot(1, 2, 1, projection='3d')
    ax.plot_surface(X,Y,Z,rstride = 5, cstride = 5, cmap = 'rainbow', alpha = .4)
    ax.plot(GD_X,GD_Y, GD_Z,color = 'b', marker = '*', alpha = .6)
    
    ax.view_init(45, 240)
    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_zlabel('z')
    
    #Contour plot
    ax = fig.add_subplot(1, 2, 2)
    C =ax.contour(X,Y,Z,20,colors="K")
    ax.clabel(C, fontsize=9, inline=1)
    ax.contourf(X,Y,Z,20)    
    ax.plot(GD_X,GD_Y,marker="*",color="R") # point and path   
    ax.set_title('Gradient Descent with {} iterations'.format(iter_num))    
    plt.show()

##############\ Rosenbrock Function - Neserov Accelerated Gradient ##########
    # save value of X，Y,Z
    GD_X = []
    GD_Y = []
    GD_Z = []
    # set the initial value of X and Y
    x = 2.5
    y = 5
    Vx = 0 #initial velociay
    Vy = 0
    alpha=bin_search(alpha_min=0.0001, alpha_max=0.00085, x0=2.5, y0=5)
    # set the initial value of Z
    f_change = f(x,y) # record the Z value
    f_current = f_change # set the initial value of Z
    
    #record the value 
    GD_X.append(x)
    GD_Y.append(y)
    GD_Z.append(f_current)
    
    #interation 
    iter_num=0
    while f_change > 1e-11 and iter_num < 200000:
        iter_num +=1
        pre_x = x
        pre_y = y
        pre_Vx = Vx
        pre_Vy= Vy 
    
        Vx = 0.9*pre_Vx + alpha * dx((pre_x - 0.9*pre_Vx), (pre_y - 0.9*pre_Vy))
        Vy = 0.9*pre_Vy + alpha * dy((pre_x - 0.9*pre_Vx), (pre_y - 0.9*pre_Vy))
        x = pre_x - Vx
        y = pre_y - Vy
    
        tmp = f(x,y)
        f_change = np.abs(f_current - tmp)
        f_current = tmp
        GD_X.append(x)
        GD_Y.append(y)
        GD_Z.append(f_current)
    steps = iter_num-1
    print("\nNeserov Accelerated Gradient")
    print(("x = %.5f,y = %.5f,f(x,y)= %.5f") %(x,y,f_current))
    print("Number of steps： %d" %steps)
    
    
    
    fig = plt.figure(figsize = (16,8))
    x = np.linspace(-3.1,3.1,250)
    y = np.linspace(-4,7,250)
    X, Y = np.meshgrid(x, y)
    Z = f(X, Y)
    
    #Surface plot
    ax = fig.add_subplot(1, 2, 1, projection='3d')
    ax.plot_surface(X,Y,Z,rstride = 5, cstride = 5, cmap = 'rainbow', alpha = .4)
    ax.plot(GD_X,GD_Y, GD_Z,color = 'b', marker = '*', alpha = .6)
    
    ax.view_init(45, 240)
    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_zlabel('z')
    
    #Contour plot
    ax = fig.add_subplot(1, 2, 2)
    C =ax.contour(X,Y,Z,20,colors="K")
    ax.clabel(C, fontsize=9, inline=1)
    ax.contourf(X,Y,Z,20)    
    ax.plot(GD_X,GD_Y,marker="*",color="R") # point and path   
    ax.set_title('Gradient Descent with {} iterations'.format(iter_num))    
    plt.show()



###################### Three-hump camel function #############        
elif Function == 3:
    print("\n######  Three-hump camel function  ######")    
    def f(x,y):
        return 2*x*x - 1.05*(x**4) + (x**6)/6 + x*y + y**2  
                                 
    
    def dx(x,y):
        return 4*x-4.2*(x**3) + x**5 + y
    
    def dy(x,y):
        return x+2*y
    
    
 ####################### Three-hump camel function - standard##############   
       
    # save value of X，Y,Z
    GD_X = []
    GD_Y = []
    GD_Z = []
    # set the initial value of X and Y
    x = 4
    y = 5
    print(("\nThe initial location: x = %.5f, y = %.5f")%(x,y))
    alpha=bin_search(alpha_min=0.0005, alpha_max=0.005, x0=4, y0=5)
    print("The initial step-size: %.5f" %alpha)
    # set the initial value of Z
    f_change = f(x,y) # record the Z value
    f_current = f_change # set the initial value of Z
    
    #record the value 
    GD_X.append(x)
    GD_Y.append(y)
    GD_Z.append(f_current)
    
    #interation 
    iter_num=0
    while f_change > 1e-10 and iter_num < 200000:
        iter_num +=1
        pre_x = x
        pre_y = y
        x = x - alpha * dx(pre_x,pre_y)
        y = y - alpha * dy(pre_x,pre_y)
    
        tmp = f(x,y)
        f_change = np.abs(f_current - tmp)
        f_current = tmp
        GD_X.append(x)
        GD_Y.append(y)
        GD_Z.append(f_current)
    steps = iter_num-1
    print("\nStandard")
    print(("x = %.5f,y = %.5f,f(x,y)= %.5f") %(x,y,f_current))
    print("Number of steps： %d" %steps)
    
    

    fig = plt.figure(figsize = (16,8))
    x = np.linspace(-5,5,250)
    y = np.linspace(-5,5.5,250)
    X, Y = np.meshgrid(x, y)
    Z = f(X, Y)

    
    #Surface plot
    ax = fig.add_subplot(1, 2, 1, projection='3d')
    ax.plot_surface(X,Y,Z,rstride = 5, cstride = 5, cmap = 'rainbow', alpha = .4)
    ax.plot(GD_X,GD_Y, GD_Z,color = 'b', marker = '*', alpha = .6)
    
    ax.view_init(40, 295)
    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_zlabel('z')
    
    #Contour plot
    ax = fig.add_subplot(1, 2, 2)
    ax.contourf(X,Y,Z,20)
    C =ax.contour(X,Y,Z,20,colors="K")
    ax.clabel(C, fontsize=9, inline=1)
    ax.plot(GD_X,GD_Y,marker="*",color="r") # point and path
    ax.set_title('Gradient Descent with {} iterations'.format(iter_num))
    plt.tight_layout()
    plt.show()   
        
        
    
################################ Three-hump camel function - momentum #############


    # save value of X，Y,Z
    GD_X = []
    GD_Y = []
    GD_Z = []
    # set the initial value of X and Y
    x = 4
    y = 5
    Vx = 0 #initial velociay
    Vy = 0
    alpha=bin_search(alpha_min=0.0005, alpha_max=0.005, x0=4, y0=5)
    # set the initial value of Z
    f_change = f(x,y) # record the Z value
    f_current = f_change # set the initial value of Z
    
    #record the value 
    GD_X.append(x)
    GD_Y.append(y)
    GD_Z.append(f_current)
    
    #interation 
    iter_num=0
    while f_change > 1e-10 and iter_num < 20000:
        iter_num +=1
        pre_x = x
        pre_y = y
        pre_Vx = Vx
        pre_Vy= Vy 
    
        Vx = 0.9*pre_Vx + alpha * dx(pre_x,pre_y)
        Vy = 0.9*pre_Vy + alpha * dy(pre_x,pre_y)
        x = pre_x - Vx
        y = pre_y - Vy
    
        tmp = f(x,y)
        f_change = np.abs(f_current - tmp)
        f_current = tmp
        GD_X.append(x)
        GD_Y.append(y)
        GD_Z.append(f_current)
    steps = iter_num-1
    print("\nMomentum")
    print(("x = %.5f,y = %.5f,f(x,y)= %.5f") %(x,y,f_current))
    print("Number of steps ： %d" %steps )
    
    

    fig = plt.figure(figsize = (16,8))
    x = np.linspace(-5,5,250)
    y = np.linspace(-5,5.5,250)
    X, Y = np.meshgrid(x, y)
    Z = f(X, Y)
    
    
    #Surface plot
    ax = fig.add_subplot(1, 2, 1, projection='3d')
    ax.plot_surface(X,Y,Z,rstride = 5, cstride = 5, cmap = 'rainbow', alpha = .4)
    ax.plot(GD_X,GD_Y, GD_Z,color = 'b', marker = '*', alpha = .6)
    
    ax.view_init(40, 295)
    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_zlabel('z')
    
    #Contour plot
    ax = fig.add_subplot(1, 2, 2)
    ax.contourf(X,Y,Z,20)
    C =ax.contour(X,Y,Z,20,colors="K")
    ax.clabel(C, fontsize=9, inline=1)
    ax.plot(GD_X,GD_Y,marker="*",color="r") # point and path
    ax.set_title('Gradient Descent with {} iterations'.format(iter_num))
    plt.tight_layout()
    plt.show()


    
################ Three-hump camel function - Neserov Accelerated Gradient  ########
    # save value of X，Y,Z
    GD_X = []
    GD_Y = []
    GD_Z = []
    # set the initial value of X and Y
    x = 4
    y = 5
    Vx = 0 #initial velociay
    Vy = 0
    alpha=bin_search(alpha_min=0.0005, alpha_max=0.005, x0=4, y0=5)
    # set the initial value of Z
    f_change = f(x,y) # record the Z value
    f_current = f_change # set the initial value of Z
    
    #record the value 
    GD_X.append(x)
    GD_Y.append(y)
    GD_Z.append(f_current)
    

    #interation 
    iter_num=0
    while f_change > 1e-10 and iter_num < 20000:
        iter_num +=1
        pre_x = x
        pre_y = y
        pre_Vx = Vx
        pre_Vy= Vy 
    
        Vx = 0.9*pre_Vx + alpha * dx((pre_x - 0.9*pre_Vx), (pre_y - 0.9*pre_Vy))
        Vy = 0.9*pre_Vy + alpha * dy((pre_x - 0.9*pre_Vx), (pre_y - 0.9*pre_Vy))
        x = pre_x - Vx
        y = pre_y - Vy
    
        tmp = f(x,y)
        f_change = np.abs(f_current - tmp)
        f_current = tmp
        GD_X.append(x)
        GD_Y.append(y)
        GD_Z.append(f_current)
    steps = iter_num-1
    print("\nNeserov Accelerated Gradient")
    print(("x = %.5f,y = %.5f,f(x,y)= %.5f") %(x,y,f_current))
    print("Number of steps： %d" %steps)

    fig = plt.figure(figsize = (16,8))
    x = np.linspace(-5,5,250)
    y = np.linspace(-5,5,250)
    X, Y = np.meshgrid(x, y)
    Z = f(X, Y)

    #Surface plot
    ax = fig.add_subplot(1, 2, 1, projection='3d')
    ax.plot_surface(X,Y,Z,rstride = 5, cstride = 5, cmap = 'rainbow', alpha = .4)
    ax.plot(GD_X,GD_Y, GD_Z,color = 'b', marker = '*', alpha = .6)
    ax.view_init(40, 295)
    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_zlabel('z')
    
    #Contour plot
    ax = fig.add_subplot(1, 2, 2)
    ax.contourf(X,Y,Z,20)
    C =ax.contour(X,Y,Z,20,colors="K")
    ax.clabel(C, fontsize=9, inline=1)
    ax.plot(GD_X,GD_Y,marker="*",color="r") # point and path
    ax.set_title('Gradient Descent with {} iterations'.format(iter_num))
    plt.tight_layout()
    plt.show()

else:
    print("\nError: Please typing correct Function number")


