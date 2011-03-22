package test;

public abstract class AsyncTask<Params, Progress, Result> {
    protected abstract Result doInBackground(Params... args);
}