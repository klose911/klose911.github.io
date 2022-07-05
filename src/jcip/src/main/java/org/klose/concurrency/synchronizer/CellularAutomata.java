/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.klose.concurrency.synchronizer;

import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;

public class CellularAutomata {

    private final Board mainBoard;
    //栅栏用来保证每个子线程计算完毕
    private final CyclicBarrier barrier;
    //子计算线程用来计算每一个小的区域
    private final Worker[] workers;

    public CellularAutomata(Board board) {
        this.mainBoard = board;
        int count = Runtime.getRuntime().availableProcessors();
        //创建等于cpu核心数量的栅栏，
        this.barrier = new CyclicBarrier(count, () -> {
            //当所有的子线程都计算完毕后，执行下面语句
            mainBoard.commitNewValues();
        });
        this.workers = new Worker[count];
        //创建所有的计算子线程
        for (int i = 0; i < count; i++) {
            workers[i] = new Worker(mainBoard.getSubBoard(count, i));
        }
    }

    private static class Board {
        private int[][] values; 
        
        public Board() {
        }

        private void commitNewValues() {
            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }

        private Board getSubBoard(int count, int i) {
            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }

        private boolean hasConverged() {
            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }

        private int getMaxX() {
            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }

        private int getMaxY() {
            return values[0].length;
        }

        private void waitForConvergence() {
            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }
        
        private void setNewValue(int x, int y, int value) {
            values[x][y] = value;
        }
    }

    private class Worker implements Runnable {

        private final Board board;

        public Worker(Board board) {
            this.board = board;
        }

        public void run() {
            while (!board.hasConverged()) {
                for (int x = 0; x < board.getMaxX(); x++) {
                    for (int y = 0; y < board.getMaxY(); y++) {
                        board.setNewValue(x, y, computeValue(x, y));
                    }
                }
                try {
                    //通知栅栏当前子线程的计算工作已经完成
                    barrier.await();
                } catch (InterruptedException ex) {
                    return;
                } catch (BrokenBarrierException ex) {
                    return;
                }
            }
        }

        private int computeValue(int x, int y) {
                return x + y;
        }
    }

    public void start() {
        //手动启动所有的计算子线程
        for (int i = 0; i < workers.length; i++) {
            new Thread(workers[i]).start();
        }
        //执行完毕栅栏中的run代码后
        mainBoard.waitForConvergence();
    }
}
