#include<cstdio>
#include<algorithm>
#include<cstring>
#include<iostream>
using namespace std;
const int MaxSize=1e5+10086;
int n,m,r,p,mod;
inline int read(){
    int ans=0,b=1;char ch=getchar();
    while(ch<'0'||ch>'9'){if(ch=='-')b=-1;else b=1;ch=getchar();}
    while(ch>='0'&&ch<='9'){ans=(ans<<3)+(ans<<1)+ch-'0';ch=getchar();}
    return ans*b;
} 
class Tree{
    public:
        void addValue(int x,int v){
            node[x].v=v;
        }
        void addEdge(int x,int y){
            nex[++tot]=fir[x];
            fir[x]=tot;
            to[tot]=y;
            nex[++tot]=fir[y];
            fir[y]=tot;
            to[tot]=x;
        }
        void dfs(int x,int f){
            int MaxSon=-1;
            node[x].depth=node[f].depth+1;
            node[x].fa=f;
            node[x].size=1;
            for(int i=fir[x];i;i=nex[i]){
                int v=to[i];
                if(v==f)continue;
                dfs(v,x);
                node[x].size+=node[v].size;
                if(node[v].size>MaxSon)node[x].son=v,MaxSon=node[v].size;
            }
        }
    protected:
        struct Node{
            int depth,fa,son,size,v;
            int id,top;
        }node[MaxSize];
        int fir[MaxSize],nex[MaxSize<<1],to[MaxSize<<1],tot;
};
class CutTree:public Tree{
    public:
        void buildCutTree(int x,int top){
            node[x].id=++si;
            va[si]=node[x].v;
            node[x].top=top;
            if(!node[x].son)return;
            buildCutTree(node[x].son,top);
            for(int i=fir[x];i;i=nex[i]){
                int v=to[i];
                if(v==node[x].fa||v==node[x].son)continue;
                buildCutTree(v,v);
            }
        }
        void updRange(int x,int y,int v);
        void updSon(int x,int y);
        int queryRange(int x,int y);
        int querySon(int x);
    protected:
        int va[MaxSize];
        int si;
};
class SegTree:public CutTree{
    public:
        int query(int k,int l,int r){
            int sl=segnode[k].l;
            int sr=segnode[k].r;
            if(l<=sl&&r>=sr)return segnode[k].sum;
            int ans=0;
            pushDown(k);
            int mid=(sl+sr)>>1;
            if(l<=mid)ans+=query(k<<1,l,r);ans%=mod;
            if(r>mid)ans+=query(k<<1|1,l,r);ans%=mod;
            return ans;
        }
        void upd(int k,int l,int r,int plus,int time){
            int sl=segnode[k].l;
            int sr=segnode[k].r;
            if(l<=sl&&r>=sr){
                goAdd(k,sr-sl+1,plus,time);
                goMod(k);
                return;
            }
            pushDown(k);
            int mid=(sl+sr)>>1;
            if(l<=mid)upd(k<<1,l,r,plus,time);
            if(r>mid)upd(k<<1|1,l,r,plus,time);
            pushUp(k);
        }
        void buildSegTree(int k,int l,int r){
            segnode[k]=(SegNode){0,l,r,0,1};
            if(l==r){
                segnode[k].sum=va[l];return;
            }
            int mid=(l+r)>>1;
            if(l<=mid)buildSegTree(k<<1,l,mid);
            if(r>mid)buildSegTree(k<<1|1,mid+1,r);
            pushUp(k);
        }
    private:
        struct SegNode{
            int sum,l,r;
            int LazyTagPlus,LazyTagTime;
        }segnode[MaxSize<<2];
        void pushUp(int k){
            segnode[k].sum=segnode[k<<1].sum+segnode[k<<1|1].sum;
        }
        void goAdd(int k,int range,int plus,int time){
            segnode[k].LazyTagTime*=time;
            segnode[k].LazyTagPlus*=time;
            segnode[k].LazyTagPlus+=plus;
            segnode[k].sum*=time;
            segnode[k].sum+=plus*range;
        }
        void goMod(int k){
            segnode[k].LazyTagPlus%=mod;
            segnode[k].LazyTagTime%=mod;
            segnode[k].sum%=mod;
        }
        void pushDown(int k){
            int l=segnode[k].l;
            int r=segnode[k].r;
            int mid=(l+r)>>1;
            goAdd(k<<1,mid-l+1,segnode[k].LazyTagPlus,segnode[k].LazyTagTime);
            goAdd(k<<1|1,r-mid,segnode[k].LazyTagPlus,segnode[k].LazyTagTime);
            goMod(k<<1);
            goMod(k<<1|1);
            segnode[k].LazyTagPlus=0;
            segnode[k].LazyTagTime=1;
        }
};
SegTree T;
void CutTree::updRange(int x,int y,int v){
    v%=mod;
    while(node[x].top!=node[y].top){
        if(node[node[x].top].depth<node[node[y].top].depth)swap(x,y);
        T.upd(1,node[node[x].top].id,node[x].id,v,1);
        x=node[node[x].top].fa;
    }
    if(node[x].depth>node[y].depth)swap(x,y);
    T.upd(1,node[x].id,node[y].id,v,1);
}
void CutTree::updSon(int x,int v){
    T.upd(1,node[x].id,node[x].id+node[x].size-1,v,1);
}
int CutTree::queryRange(int x,int y){
	int ans=0;
	while(node[x].top!=node[y].top){
		if(node[node[x].top].depth<node[node[y].top].depth)swap(x,y);
		ans+=T.query(1,node[node[x].top].id,node[x].id);
		ans%=mod;
		x=node[node[x].top].fa;
	}
	if(node[x].depth>node[y].depth)swap(x,y);
	ans+=T.query(1,node[x].id,node[y].id);
	return ans%mod;
}
int CutTree::querySon(int x){
	return T.query(1,node[x].id,node[x].id+node[x].size-1);
}
void init(){
    n=read();m=read();r=read();p=read();mod=p;
    for(int i=1;i<=n;i++)T.addValue(i,read());
    for(int i=1;i<n;i++)T.addEdge(read(),read());
    T.dfs(r,0);
    T.buildCutTree(r,r);
    T.buildSegTree(1,1,n);
}
void work(){
    int way,x,y,z;
    for(int i=1;i<=m;i++){
        way=read();
        switch(way){
            case(1):{
                x=read();y=read();z=read();
                T.updRange(x,y,z);
                break;}
            case(2):{
                x=read();y=read();
                printf("%d\n",T.queryRange(x,y));
                break;}
            case(3):{
                x=read();z=read();
                T.updSon(x,z);
                break;}
            case(4):{
                x=read();
                printf("%d\n",T.querySon(x));
                break;}
        }
    }
}
int main(){
    init();
    work();
}
